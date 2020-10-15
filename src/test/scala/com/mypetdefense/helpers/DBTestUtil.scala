package com.mypetdefense.helpers

import java.util.Date

import com.mypetdefense.helpers.Random.{generateMoneyString, generateString}
import com.mypetdefense.model._
import net.liftweb.common.{Box, Empty, Full}

object DBTestUtil {

  def createUser(
      firstName: String = generateString,
      lastName: String = generateString,
      stripeId: String = generateString,
      email: String = generateString,
      password: String = generateString,
      phone: String = "123-123-1234",
      coupon: Box[Coupon] = Empty,
      referer: Box[Agency] = Empty,
      agency: Box[Agency] = Empty,
      userType: UserType.Value = UserType.Parent
  ): User =
    User.createNewUser(
      firstName,
      lastName,
      stripeId,
      email,
      password,
      phone,
      coupon,
      referer,
      agency,
      userType
    )

  def createSubscription(
      parent: User,
      stripeSubscriptionId: String = generateString,
      startDate: Date = new Date(),
      nextShipDate: Date = new Date(),
      priceCode: String = Price.defaultPriceCode,
      isUpgraded: Boolean = false,
      contractLength: Int = 0
  ): Subscription = Subscription.createNewSubscription(
    Full(parent),
    stripeSubscriptionId,
    startDate,
    nextShipDate,
    priceCode,
    isUpgraded,
    contractLength
  )

  def createPet(
      user: User,
      name: String = generateString,
      animalType: AnimalType.Value,
      size: AnimalSize.Value,
      whelpDate: Box[Date] = Empty,
      breed: String = generateString
  ): Pet = Pet.createNewPet(user, name, animalType, size, whelpDate, breed)

  def createBox(subscription: Subscription, pet: Pet): SubscriptionBox =
    SubscriptionBox.createNewBox(subscription, pet)

  def createShipment(
      user: User,
      subscription: Subscription,
      stripePaymentId: String = generateString,
      stripeChargeId: Box[String],
      amountPaid: String = generateMoneyString,
      taxPaid: String = generateMoneyString,
      inserts: List[Insert],
      shipmentStatus: ShipmentStatus.Value,
      sendFreeUpgrade: Boolean = false
  ): Shipment =
    Shipment.createShipment(
      user,
      subscription,
      stripePaymentId,
      stripeChargeId,
      amountPaid,
      taxPaid,
      inserts,
      shipmentStatus,
      sendFreeUpgrade
    )

  def clearTables(): Unit = {
    User.findAll().map(_.delete_!)
    Subscription.findAll().map(_.delete_!)
  }

}
