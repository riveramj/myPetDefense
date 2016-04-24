package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._

class Subscription extends LongKeyedMapper[Subscription] with IdPK {
  def getSingleton = Subscription
  object subscriptionId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object parent extends MappedLongForeignKey(this, Parent)
  object subscriptionType extends MappedEnum(this, SubscriptionType)
  object startDate extends MappedDateTime(this)
  object renewalDate extends MappedDateTime(this)
  object nextShipDate extends MappedDateTime(this)
  object status extends MappedEnum(this, Status) {
    override def defaultValue = Status.Active
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def getProducts = Pet.findAll(By(Pet.parent, parent.get)).flatMap(_.product.obj)

  def createNewSubscription(
    parent: Parent,
    subscriptionType: SubscriptionType.Value,
    startDate: Date,
    nextShipDate: Date
  ) = {
    Subscription.create
    .subscriptionId(generateLongId)
    .parent(parent)
    .subscriptionType(subscriptionType)
    .startDate(startDate)
    .nextShipDate(nextShipDate)
    .saveMe
  }
}

object Subscription extends Subscription with LongKeyedMetaMapper[Subscription]

object SubscriptionType extends Enumeration {
  val Month, Year = Value
}

object Status extends Enumeration {
  val Active, Inactive = Value
}
