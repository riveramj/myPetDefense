package com.fleaTick.model

import net.liftweb.mapper._
import java.util.Date

class Order extends LongKeyedMapper[Order] with IdPK with OneToMany[Long, Order] {
  def getSingleton = Order
  object OrderId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object user extends MappedLongForeignKey(this, User)
  object subscription extends MappedLongForeignKey(this, Subscription)
  object total extends MappedString(this, 100)
  object orderDate extends MappedDateTime(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Order extends Order with LongKeyedMetaMapper[Order]
