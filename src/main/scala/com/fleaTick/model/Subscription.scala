package com.fleaTick.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import java.util.Date

import com.fleaTick.util.RandomIdGenerator._

class Subscription extends LongKeyedMapper[Subscription] with IdPK {
  def getSingleton = Subscription
  object subscriptionId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object parent extends MappedLongForeignKey(this, Parent)
  object pet extends MappedLongForeignKey(this, Pet)
  object product extends MappedLongForeignKey(this, Product)
  object subscriptionType extends MappedEnum(this, SubscriptionType)
  object status extends MappedEnum(this, Status) {
    override def defaultValue = Status.Active
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewSubscription(
    parent: Parent,
    pet: Pet,
    subscriptionType: SubscriptionType.Value
  ) = {
    Subscription.create
    .subscriptionId(generateLongId)
    .parent(parent)
    .pet(pet)
    .subscriptionType(subscriptionType)
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
