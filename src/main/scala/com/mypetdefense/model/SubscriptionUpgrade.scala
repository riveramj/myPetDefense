package com.mypetdefense.model

import com.mypetdefense.model.Agency.getHQFor

import java.util.Date
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
  object referrer               extends MappedLongForeignKey(this, Agency)
  object subscription           extends MappedLongForeignKey(this, Subscription)
  object upgradeDate extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object SubscriptionUpgrade
    extends SubscriptionUpgrade
    with LongKeyedMetaMapper[SubscriptionUpgrade] {
  def createSubscriptionUpgrade(
      subscription: Subscription,
      user: User,
      shipmentCount: Int
  ): SubscriptionUpgrade = {
    SubscriptionUpgrade.create
      .subscription(subscription)
      .referrer(user.referer.obj.map(getHQFor))
      .user(user)
      .shipmentCountAtUpgrade(shipmentCount)
      .saveMe
  }
}
