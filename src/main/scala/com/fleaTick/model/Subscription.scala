package com.fleaTick.model

import net.liftweb.mapper._
import java.util.Date

class Subscription extends LongKeyedMapper[Subscription] with IdPK with OneToMany[Long, Subscription] {
  def getSingleton = Subscription
  object SubscriptionId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object user extends MappedLongForeignKey(this, User)
  object pet extends MappedLongForeignKey(this, Pet)
  object order extends MappedLongForeignKey(this, Order)
  object product extends MappedLongForeignKey(this, Product)
  object subscriptionType extends MappedEnum(this, SubscriptionType)
  object status extends MappedEnum(this, Status) {
    override def defaultValue = Status.Active
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Subscription extends Subscription with LongKeyedMetaMapper[Subscription]

object SubscriptionType extends Enumeration {
  val Quarter, HalfYear, Year = Value
}

object Status extends Enumeration {
  val Active, Inactive = Value
}
