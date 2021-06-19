package com.mypetdefense.service

import com.mypetdefense.actor.{EmailActor, NewSaleEmail, SendWelcomeEmail}
import com.mypetdefense.model._
import com.mypetdefense.model.domain.action.CustomerAction.{CustomerAddedPet, CustomerSignedUp}
import com.mypetdefense.service.PetFlowChoices.priceCode
import com.mypetdefense.service.StripeFacade.Customer
import com.mypetdefense.snippet.signup.{NewUserAddress, NewUserData}
import com.mypetdefense.util.DateHelper.tomorrowStart
import com.mypetdefense.util.{DateHelper, SecurityContext}
import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.util.Props

import java.util.Date
import scala.collection.mutable

object CheckoutService {

  private def createNewPets(user: Box[User], pets: List[PendingPet]): List[PendingPet] = {
    for {
      usr <- user.toList
      pet <- pets
    } yield {
      PendingPet(Pet.createNewPet(pet.pet, usr), pet.boxType, pet.thirtyDaySupplement)
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
          UserType.Parent,
          newUserData.ipAddress
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

  private def findSubscriptionId(customer: StripeFacade.CustomerWithSubscriptions): Option[String] =
    for {
      rawSubscriptions <- customer.value.subscriptions
      subscription     <- rawSubscriptions.data.headOption
    } yield subscription.id

  private def createNewSubscription(
      user: Box[User],
      priceCode: String,
      subscriptionId: String
  ): Subscription = {
    Subscription.createNewSubscription(
      user,
      subscriptionId,
      new Date(),
      tomorrowStart,
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

  private def createNewBox(mpdSubscription: Subscription, pet: PendingPet) = {
    val box = SubscriptionBox.createNewBox(mpdSubscription, pet.pet, pet.boxType)
    val updatedPet = pet.pet.box(box).saveMe()

    PendingPet(updatedPet, pet.boxType, pet.thirtyDaySupplement, Full(box))
  }

  def tryToCreateUser(
    couponCode: String,
    petPrices: List[Price],
    coupon: Box[Coupon],
    stripeToken: Box[String],
    stripePaymentMethod: Box[String],
    email: String,
    taxRate: Double
  ) = {
    val fiveDollarPromo = List("20off", "80off").contains(couponCode)
    val subscriptionItems = petPrices.groupBy(_.stripePriceId.get).map { case (stripePriceId, prices) =>
      if (fiveDollarPromo) {
        val samplePriceSize = prices.headOption.map(_.petSize.get)
        val fiveDollarPriceId = samplePriceSize
          .flatMap(Price.getFiveDollarPriceCode)
          .map(_.stripePriceId.get)
          .getOrElse("")

        StripeFacade.Subscription.Item(fiveDollarPriceId, prices.size)
      } else
        StripeFacade.Subscription.Item(stripePriceId, prices.size)
    }

    val promoCoupon = if (fiveDollarPromo) Empty else coupon

    val stripeCustomer = {
      Customer.createWithSubscription(
        email,
        stripeToken,
        stripePaymentMethod,
        taxRate,
        promoCoupon,
        subscriptionItems.toList
      )
    }

    stripeCustomer
  }

  def updateSessionVars(petCount: Int, monthlyTotal: BigDecimal, todayTotal: BigDecimal) = {
    PetFlowChoices.petCount(Full(petCount))
    PetFlowChoices.cart(mutable.LinkedHashMap.empty)
    PetFlowChoices.monthlyTotal(Full(monthlyTotal))
    PetFlowChoices.todayTotal(Full(todayTotal))
  }

  def setupNewUser(
    customer: StripeFacade.CustomerWithSubscriptions,
    petsToCreate: List[PendingPet],
    newUserData: NewUserData,
    couponCode: Box[String]
  ) = {
    val priceCodeOfSubscription = priceCode.is.openOr(Price.defaultPriceCode)
    val userWithSubscription = newUserSetup(
      Empty,
      petsToCreate,
      priceCodeOfSubscription,
      newUserData,
      customer
    )
    val updatedUserWithSubscription = userWithSubscription.map(_.reload)
    updatedUserWithSubscription.map(SecurityContext.logIn)

    for {
      offerCode <- PetFlowChoices.woofTraxOfferCode.is
      userId <- PetFlowChoices.woofTraxUserId.is
      user <- updatedUserWithSubscription
    } yield {
      WoofTraxOrder.createWoofTraxOrder(offerCode, userId, user)
    }

    val todayDateTime = DateHelper.now.toString
    val signUpActionLog = CustomerSignedUp(
      SecurityContext.currentUserId,
      None,
      todayDateTime,
      couponCode
    )
    val petActionLog = updatedUserWithSubscription.toList
      .flatMap(_.pets.toList)
      .map { pet =>
        CustomerAddedPet(
          SecurityContext.currentUserId,
          None,
          pet.petId.get,
          pet.name.get
        )
      }

    ActionLogService.logAction(signUpActionLog)
    petActionLog.foreach(ActionLogService.logAction)
  }

  def newUserSetup(
      maybeCurrentUser: Box[User],
      petsToCreate: List[PendingPet],
      priceCode: String,
      newUserData: NewUserData,
      customer: StripeFacade.CustomerWithSubscriptions
  ): Box[User] = {
    val stripeId = customer.value.id
    val coupon   = newUserData.coupon
    val petCount = petsToCreate.size
    val user     = createUserOrUpdate(maybeCurrentUser, newUserData, stripeId, coupon)

    createAddress(user, newUserData.address)

    val pets = createNewPets(user, petsToCreate)

    val subscriptionId = findSubscriptionId(customer).getOrElse("")

    val mpdSubscription = createNewSubscription(user, priceCode, subscriptionId)

    val userWithSubscription = user.map(_.subscription(mpdSubscription).saveMe())

    val boxes = pets.map(createNewBox(mpdSubscription, _))

    boxes.map(SubscriptionItem.createNewBox)

    sendCheckoutEmails(userWithSubscription, petCount, coupon)

    userWithSubscription
  }

}
