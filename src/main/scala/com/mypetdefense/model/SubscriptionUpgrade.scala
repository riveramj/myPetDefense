package com.mypetdefense.model

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.mapper._

class SubscriptionUpgrade extends LongKeyedMapper[SubscriptionUpgrade] with IdPK {
  def getSingleton: KeyedMetaMapper[Long, SubscriptionUpgrade] = SubscriptionUpgrade

  object subscriptionUpgradeId extends MappedLong(this) {
    override def dbIndexed_?        = true
    override def defaultValue: Long = generateLongId
  }
  object shipmentCountAtUpgrade extends MappedInt(this)
  object user                   extends MappedLongForeignKey(this, User)
  object subscription           extends MappedLongForeignKey(this, Subscription)
  object subscriptionBox        extends MappedLongForeignKey(this, SubscriptionBox)
  object upgradeDate            extends MappedZonedDateTime(this, useNowAsDefault = true)
  object createdAt              extends MappedZonedDateTime(this, useNowAsDefault = true)
}

object SubscriptionUpgrade
    extends SubscriptionUpgrade
    with LongKeyedMetaMapper[SubscriptionUpgrade] {
  def createSubscriptionUpgrade(
      subscription: Subscription,
      subscriptionBox: SubscriptionBox,
      user: User,
      shipmentCount: Int
  ): SubscriptionUpgrade = {
    SubscriptionUpgrade.create
      .subscription(subscription)
      .subscriptionBox(subscriptionBox)
      .user(user)
      .shipmentCountAtUpgrade(shipmentCount)
      .saveMe
  }
}
