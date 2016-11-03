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

object ParentService extends Loggable {
  val stripeSecretKey = Props.get("secret.key") openOr ""
  implicit val e = new StripeExecutor(stripeSecretKey)

  def updateStripeSubscriptionQuantity(customerId: String, subscriptionId: String, quantity: Int) = {
    StripeSubscription.update(
      customerId = customerId,
      subscriptionId = subscriptionId,
      quantity = Some(quantity),
      prorate = Some(false)
    )
  }

  def deleteStripeCustomer(customerId: String) = {
    Customer.delete(customerId)
  }

  def removeParent(oldUser: User): Box[User] = {
    val user = User.find(By(User.userId, oldUser.userId.get))

    val removeCustomer = deleteStripeCustomer(user.map(_.stripeId.get).openOr(""))

    Try(Await.result(removeCustomer, new DurationInt(10).seconds)) match {
      case TrySuccess(Full(stripeSub)) =>
        val subscription = user.flatMap(_.getSubscription)


        val shipments = subscription.map(_.shipments.toList).openOr(Nil)
        val addresses = user.map(_.addresses.toList).openOr(Nil)

        shipments.map(_.delete_!)
        addresses.map(_.delete_!)
        subscription.map(_.delete_!)
        user.map(_.delete_!)

        user

      case TrySuccess(stripeFailure) =>
        logger.error(s"remove customer failed with stipe error: ${stripeFailure}")
        logger.error(s"trying to delete ${user} anyways")
        logger.error(s"no promises this works. Check data tables")

        val subscription = user.flatMap(_.getSubscription)
        val shipments = subscription.map(_.shipments.toList).openOr(Nil)
        val addresses = user.map(_.addresses.toList).openOr(Nil)

        shipments.map(_.delete_!)
        addresses.map(_.delete_!)
        subscription.map(_.delete_!)
        user.map(_.delete_!)

        user

      case TryFail(throwable: Throwable) =>
        logger.error(s"remove customer failed with other error: ${throwable}")
        Empty
    }
  }
}

