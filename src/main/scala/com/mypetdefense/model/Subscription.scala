package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._

class Subscription extends LongKeyedMapper[Subscription] with IdPK with OneToMany[Long, Subscription] {
  def getSingleton = Subscription
  object subscriptionId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object user extends MappedLongForeignKey(this, User)
  object stripeSubscriptionId extends MappedString(this, 100)
  object startDate extends MappedDateTime(this)
  object renewalDate extends MappedDateTime(this)
  object nextShipDate extends MappedDateTime(this)
  object shipments extends MappedOneToMany(Shipment, Shipment.subscription)
  object status extends MappedEnum(this, Status) {
    override def defaultValue = Status.Active
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def getProducts = Pet.findAll(By(Pet.user, user.get)).flatMap(_.product.obj)

  def createNewSubscription(
    user: User,
    stripeSubscriptionId: String,
    startDate: Date,
    nextShipDate: Date
  ) = {
    Subscription.create
    .subscriptionId(generateLongId)
    .user(user)
    .stripeSubscriptionId(stripeSubscriptionId)
    .startDate(startDate)
    .nextShipDate(nextShipDate)
    .saveMe
  }
}

object Subscription extends Subscription with LongKeyedMetaMapper[Subscription]

object Status extends Enumeration {
  val Active, Inactive = Value
}
