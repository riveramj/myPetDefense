package com.mypetdefense.model

import net.liftweb.mapper._

import com.mypetdefense.util.RandomIdGenerator._
import java.util.Date

class BoxOrder extends LongKeyedMapper[BoxOrder] with IdPK with OneToMany[Long, BoxOrder] {
  def getSingleton = BoxOrder
  object boxOrderId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object user extends MappedLongForeignKey(this, User)
  object stripeChargeId extends MappedString(this, 100)
  object trackingNumber extends MappedString(this, 100)
  object address extends MappedString(this, 200)
  object dateProcessed extends MappedDateTime(this)
  object expectedShipDate extends MappedDateTime(this)
  object dateShipped extends MappedDateTime(this)
  object dateReceived extends MappedDateTime(this)
  object taxPaid extends MappedString(this, 100)
  object amountPaid extends MappedString(this, 100)
  object quantity extends MappedInt(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def refresh = BoxOrder.find(By(BoxOrder.boxOrderId, boxOrderId.get))
}

object BoxOrder extends BoxOrder with LongKeyedMetaMapper[BoxOrder] {
  def createBoxOrder(
    user: User,
    stripeChargeId: String,
    amountPaid: String,
    taxPaid: String,
    quantity: Int
  ) = {
    val dateProcessed = new Date()

    BoxOrder.create
      .boxOrderId(generateLongId)
      .stripeChargeId(stripeChargeId)
      .user(user)
      .dateProcessed(dateProcessed)
      .amountPaid(amountPaid)
      .taxPaid(taxPaid)
      .quantity(quantity)
      .saveMe
  }
}
