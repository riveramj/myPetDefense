package com.mypetdefense.service 

import net.liftweb._
  import common._
  import mapper._
  import util._
  import util.Helpers._

import com.mypetdefense.model._

import me.frmr.stripe.{StripeExecutor, Customer, Coupon => StripeCoupon, Subscription => StripeSubscription}
import scala.util.{Failure => TryFail, Success => TrySuccess, _}

import com.mypetdefense.model._

import scala.concurrent.Await
import scala.concurrent.duration._

import dispatch._, Defaults._

object PetService extends Loggable {
  val stripeSecretKey = Props.get("secret.key") openOr ""
  implicit val e = new StripeExecutor(stripeSecretKey)

  def addNewPet(
    user: User,
    name: String,
    animalType: AnimalType.Value,
    size: AnimalSize.Value,
    product: Product
  ): Box[Pet] = {

    val updateSubscription = (
      ParentService.updateStripeSubscriptionQuantity(
        user.stripeId.get,
        user.getSubscription.map(_.stripeSubscriptionId.get).getOrElse(""),
        user.pets.size + 1
      )
    )

    Try(Await.result(updateSubscription, new DurationInt(10).seconds)) match {
      case TrySuccess(Full(stripeSub)) =>
        Full(Pet.createNewPet(
          user = user,
          name = name,
          animalType = animalType,
          size = size,
          product = product
        ))

      case TrySuccess(stripeFailure) =>
        logger.error(s"update (add) subscription failed with stipe error: ${stripeFailure}")
        Empty

      case TryFail(throwable: Throwable) =>
        logger.error(s"update (add) subscription failed with other error: ${throwable}")
        Empty
    }
  }

  def removePet(oldUser: Box[User], pet: Pet): Box[Pet] = {
    oldUser.map(user => removePet(user, pet)).openOr(Empty)
  }

  def removePet(oldUser: User, pet: Pet): Box[Pet] = {
    val user = User.find(By(User.userId, oldUser.userId.get))

    val updateSubscription = {
      val subscriptionId = (
        for {
          updatedUser <- user
          subscription <- updatedUser.getSubscription
        } yield {
          subscription.stripeSubscriptionId.get
        }
      ).openOr("")

      ParentService.updateStripeSubscriptionQuantity(
        user.map(_.stripeId.get).openOr(""),
        subscriptionId,
        user.map(_.pets.size - 1).openOr(0)
      )
    }

    Try(Await.result(updateSubscription, new DurationInt(10).seconds)) match {
      case TrySuccess(Full(stripeSub)) =>
        Full(pet.status(Status.Inactive).saveMe)

      case TrySuccess(stripeFailure) =>
        logger.error(s"update (remove) subscription failed with stipe error: ${stripeFailure}")
        logger.error(s"trying to delete ${pet} anyways")

        Full(pet.status(Status.Inactive).saveMe)

      case TryFail(throwable: Throwable) =>
        logger.error(s"update (remove) subscription failed with other error: ${throwable}")
        Empty
    }
  }
}
