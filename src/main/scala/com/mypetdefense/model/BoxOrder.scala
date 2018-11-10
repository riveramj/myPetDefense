package com.mypetdefense.model

import net.liftweb.mapper._
import net.liftweb.common.Box

import com.mypetdefense.util.RandomIdGenerator._
import com.mypetdefense.util.TitleCase
import java.util.Date

class BoxOrder extends LongKeyedMapper[BoxOrder] with IdPK with OneToMany[Long, BoxOrder] {
  def getSingleton = BoxOrder
  object boxOrderId extends MappedLong(this){
    override def dbIndexed_? = true
    override def defaultValue = generateLongId
  }
  object user extends MappedLongForeignKey(this, User)
  object firstName extends MappedString(this, 100)
  object lastName extends MappedString(this, 100)
  object email extends MappedString(this, 100)
  object street1 extends MappedString(this, 100)
  object street2 extends MappedString(this, 100)
  object city extends MappedString(this, 100)
  object state extends MappedString(this, 100)
  object zip extends MappedString(this, 100)
  object stripeChargeId extends MappedString(this, 100)
  object trackingNumber extends MappedString(this, 100)
  object dateProcessed extends MappedDateTime(this)
  object expectedShipDate extends MappedDateTime(this)
  object dateShipped extends MappedDateTime(this)
  object dateReceived extends MappedDateTime(this)
  object taxPaid extends MappedDouble(this)
  object amountPaid extends MappedDouble(this)
  object boxesOrdered extends MappedOneToMany(BoxOrderLineItem, BoxOrderLineItem.order)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def refresh = BoxOrder.find(By(BoxOrder.boxOrderId, boxOrderId.get))
}

object BoxOrder extends BoxOrder with LongKeyedMetaMapper[BoxOrder] {
  def createBoxOrder(
    user: Box[User],
    firstName: String,
    lastName: String,
    email: String,
    address: Address,
    stripeChargeId: String,
    amountPaid: Double,
    taxPaid: Double,
    boxes: List[(PetBox, Int)]
  ) = {
    val dateProcessed = new Date()

    val newBoxOrder = BoxOrder.create
      .boxOrderId(generateLongId)
      .stripeChargeId(stripeChargeId)
      .user(user)
      .firstName(firstName)
      .lastName(lastName)
      .email(email)
      .street1(TitleCase(address.street1.get))
      .street2(TitleCase(address.street2.get))
      .city(TitleCase(address.city.get))
      .state(address.state.get.toUpperCase)
      .zip(zip.get)
      .dateProcessed(dateProcessed)
      .amountPaid(amountPaid)
      .taxPaid(taxPaid)
      .saveMe

    boxes.map { box =>
      BoxOrderLineItem.createBoxOrderLineItems(boxes, newBoxOrder)
    }

    newBoxOrder
  }
}

class BoxOrderLineItem extends LongKeyedMapper[BoxOrderLineItem] with IdPK {
  def getSingleton = BoxOrderLineItem
  object orderLineItemId extends MappedLong(this){
    override def dbIndexed_? = true
    override def defaultValue = generateLongId
  }

  object order extends MappedLongForeignKey(this, BoxOrder)
  object box extends MappedLongForeignKey(this, PetBox)
  object quantity extends MappedInt(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createBoxOrderLineItems(
    boxes: List[(PetBox, Int)],
    order: BoxOrder
  ) = {
    boxes.map { case (box, quantity) =>
      BoxOrderLineItem.create
      .quantity(quantity)
      .box(box)
      .order(order)
      .saveMe
    }
  }
}

object BoxOrderLineItem extends BoxOrderLineItem with LongKeyedMetaMapper[BoxOrderLineItem]
