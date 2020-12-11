package com.mypetdefense.service

import java.util.Date

import com.mypetdefense.actor.{EmailActor, NewSaleEmail, SendWelcomeEmail}
import com.mypetdefense.model._
import com.mypetdefense.snippet.signup.{NewUserAddress, NewUserData}
import com.stripe.model.Customer
import net.liftweb.common.{Box, Full}
import net.liftweb.util.Props

import scala.collection.JavaConverters._

object CheckoutService {

  private def createNewPets(user: Box[User], pets: List[Pet]): List[Pet] = {
    for {
      usr <- user.toList
      pet <- pets
    } yield {
      Pet.createNewPet(pet, usr)
    }
  }

  private def createAddress(user: Box[User], address: NewUserAddress): Address = {
    Address.createNewAddress(
      user,
      address.street1,
      address.street2,
      address.city,
      address.state,
      address.zip,
      AddressType.Shipping
    )
  }

  private def createUserOrUpdate(
      maybeCurrentUser: Box[User],
      newUserData: NewUserData,
      stripeId: String,
      coupon: Box[Coupon]
  ): Box[User] =
    if (maybeCurrentUser.isEmpty) {
      Full(
        User.createNewUser(
          newUserData.firstName,
          newUserData.lastName,
          stripeId,
          newUserData.email,
          newUserData.password,
          "",
          coupon,
          coupon.flatMap(_.agency.obj),
          None,
          UserType.Parent
        )
      )
    } else {
      maybeCurrentUser.map(_.reload).map { oldUser =>
        oldUser
          .firstName(newUserData.firstName)
          .lastName(newUserData.lastName)
          .stripeId(stripeId)
          .email(newUserData.email)
          .coupon(coupon)
          .referer(coupon.flatMap(_.agency.obj))
          .saveMe
      }
    }

  private def findSubscriptionId(customer: Customer): Option[String] =
    for {
      rawSubscriptions <- Option(customer.getSubscriptions)
      subscription     <- rawSubscriptions.getData.asScala.headOption
      result           <- Option(subscription.getId)
    } yield result

  private def createNewSubscription(
      user: Box[User],
      priceCode: String,
      subscriptionId: String
  ): Subscription = {
    Subscription.createNewSubscription(
      user,
      subscriptionId,
      new Date(),
      new Date(),
      priceCode,
      isUpgraded = true
    )
  }

  private def sendCheckoutEmails(
      userWithSubscription: Box[User],
      petCount: Int,
      coupon: Box[Coupon]
  ): Unit = {
    if (Props.mode == Props.RunModes.Production) {
      EmailActor ! NewSaleEmail(
        userWithSubscription,
        petCount,
        coupon.map(_.couponCode.get).openOr("")
      )
    }

    EmailActor ! SendWelcomeEmail(userWithSubscription)
  }

  private def createNewBox(mpdSubscription: Subscription, pet: Pet) = {
    val box = SubscriptionBox.createNewBox(mpdSubscription, pet, true)
    pet.box(box).saveMe()

    box
  }

  def newUserSetup(
      maybeCurrentUser: Box[User],
      petsToCreate: List[Pet],
      priceCode: String,
      newUserData: NewUserData,
      customer: Customer
  ): Box[User] = {
    val stripeId = customer.getId
    val coupon   = newUserData.coupon
    val petCount = petsToCreate.size
    val user     = createUserOrUpdate(maybeCurrentUser, newUserData, stripeId, coupon)

    createAddress(user, newUserData.address)

    val pets = createNewPets(user, petsToCreate)

    val subscriptionId = findSubscriptionId(customer).getOrElse("")

    val mpdSubscription = createNewSubscription(user, priceCode, subscriptionId)

    val userWithSubscription = user.map(_.subscription(mpdSubscription).saveMe())

    val boxes = pets.map(createNewBox(mpdSubscription, _))

    boxes.map(SubscriptionItem.createFirstBox)

    sendCheckoutEmails(userWithSubscription, petCount, coupon)

    userWithSubscription
  }

}
