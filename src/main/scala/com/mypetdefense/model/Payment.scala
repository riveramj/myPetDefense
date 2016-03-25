package com.mypetdefense.model

import net.liftweb.mapper._
import java.util.Date

class Payment extends LongKeyedMapper[Payment] with IdPK with OneToMany[Long, Payment] {
  def getSingleton = Payment
  object PaymentId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object order extends MappedLongForeignKey(this, Order)
  object paymentToken extends MappedString(this, 100)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Payment extends Payment with LongKeyedMetaMapper[Payment]

