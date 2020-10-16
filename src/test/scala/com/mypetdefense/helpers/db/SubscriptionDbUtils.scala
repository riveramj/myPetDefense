package com.mypetdefense.helpers.db

import java.util.Date

import com.mypetdefense.helpers.Random.{genPosInt, generateString}
import com.mypetdefense.model.{Price, Subscription, User}
import net.liftweb.common.Full

object SubscriptionDbUtils {

  def createSubscription(
      parent: User,
      stripeSubscriptionId: String = generateString,
      startDate: Date = new Date(),
      nextShipDate: Date = new Date(),
      priceCode: String = Price.defaultPriceCode,
      isUpgraded: Boolean = false,
      contractLength: Int = genPosInt
  ): Subscription = Subscription.createNewSubscription(
    Full(parent),
    stripeSubscriptionId,
    startDate,
    nextShipDate,
    priceCode,
    isUpgraded,
    contractLength
  )

  def createSubscriptionCreatedAndStartedAt(user: User, creationDate: Date): Subscription =
    createSubscription(parent = user, startDate = creationDate).createdAt(creationDate).saveMe()

}
