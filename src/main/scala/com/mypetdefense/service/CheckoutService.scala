package com.mypetdefense.service

import java.util.Date

import com.mypetdefense.actor.{EmailActor, NewSaleEmail, SendWelcomeEmail}
import com.mypetdefense.model.{
  Address,
  AddressType,
  AnimalSize,
  Coupon,
  Pet,
  Price,
  Subscription,
  SubscriptionBox,
  SubscriptionItem,
  User,
  UserType
}
import com.mypetdefense.service.PetFlowChoices.{completedPets, priceCode}
import com.mypetdefense.snippet.signup.{NewUserAddress, NewUserData}
import com.mypetdefense.util.SecurityContext
import me.frmr.stripe.Customer
import net.liftweb.common.{Box, Full}
import net.liftweb.util.Props

import scala.collection.mutable

object CheckoutService {

  val pets: mutable.LinkedHashMap[Long, Pet] = completedPets.is
  val petCount: Int                          = pets.size

  val smMedPets: Int = pets.values.count { pet =>
    pet.size.get != AnimalSize.DogLargeZo && pet.size.get != AnimalSize.DogXLargeZo
  }

  private[service] def createNewPets(user: Box[User]): List[Pet] = {
    for {
      usr <- user.toList
      pet <- pets.values
    } yield {
      Pet.createNewPet(pet, usr)
    }
  }

  private[service] def createAddress(user: Box[User], address: NewUserAddress): Address = {
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

  private[service] def createUserOrUpdate(
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
      maybeCurrentUser.flatMap(_.refresh).map { oldUser =>
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

  private[service] def findSubscriptionId(customer: Customer): Option[String] =
    for {
      rawSubscriptions <- customer.subscriptions
      subscription     <- rawSubscriptions.data.headOption
      result           <- subscription.id
    } yield result

  private[service] def createNewSubscription(
      user: Box[User],
      subscriptionId: String
  ): Subscription = {
    Subscription.createNewSubscription(
      user,
      subscriptionId,
      new Date(),
      new Date(),
      priceCode.is.openOr(Price.defaultPriceCode),
      isUpgraded = true
    )
  }

  private[service] def sendCheckoutEmails(
      userWithSubscription: Box[User],
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

  private[service] def createNewBox(mpdSubscription: Subscription, pet: Pet) = {
    val box = SubscriptionBox.createNewBox(mpdSubscription, pet)
    pet.box(box).saveMe()

    box
  }

  def newUserSetup(
      maybeCurrentUser: Box[User],
      newUserData: NewUserData,
      customer: Customer
  ): Box[User] = {
    val stripeId = customer.id
    val coupon   = newUserData.coupon

    val user = createUserOrUpdate(maybeCurrentUser, newUserData, stripeId, coupon)

    createAddress(user, newUserData.address)

    val pets = createNewPets(user)

    val subscriptionId = findSubscriptionId(customer).getOrElse("")

    val mpdSubscription = createNewSubscription(user, subscriptionId)

    val userWithSubscription = user.map(_.subscription(mpdSubscription).saveMe())

    val boxes = pets.map(createNewBox(mpdSubscription, _))

    boxes.map(SubscriptionItem.createFirstBox)

    sendCheckoutEmails(userWithSubscription, coupon)

    userWithSubscription.flatMap(_.refresh).map(SecurityContext.logIn)

    userWithSubscription
  }

}
