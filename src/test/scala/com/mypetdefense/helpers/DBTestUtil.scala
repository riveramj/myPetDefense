package com.mypetdefense.helpers

import java.util.Date

import com.mypetdefense.model.{Agency, Coupon, Price, Subscription, User, UserType}
import net.liftweb.common.{Box, Empty, Full}

object DBTestUtil {

  def createUser(
      firstName: String = "Test",
      lastName: String = "User",
      stripeId: String = "stripe1234",
      email: String = "test@mpd.com",
      password: String = "pass",
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
      stripeSubscriptionId: String = "stripeSubscription1234",
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

  def createTestData(): Unit = {
    val parent1 = DBTestUtil.createUser()

    createSubscription(parent1)
  }

  def clearTables(): Unit = {
    User.findAll().map(_.delete_!)
    Subscription.findAll().map(_.delete_!)
  }

}
