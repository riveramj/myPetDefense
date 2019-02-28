package com.mypetdefense.model

import net.liftweb.mapper._
import net.liftweb.common._

import com.mypetdefense.util.RandomIdGenerator._
import com.mypetdefense.util.TitleCase
import java.util.Date

class TreatOrder extends LongKeyedMapper[TreatOrder] with IdPK with OneToMany[Long, TreatOrder] {
  def getSingleton = TreatOrder
  object treatOrderId extends MappedLong(this){
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
  object treatsOrdered extends MappedOneToMany(TreatOrderLineItem, TreatOrderLineItem.order)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def refresh = TreatOrder.find(By(TreatOrder.treatOrderId, treatOrderId.get))
}

object TreatOrder extends TreatOrder with LongKeyedMetaMapper[TreatOrder] {
  def createTreatOrder(
    user: Box[User],
    firstName: String,
    lastName: String,
    email: String,
    address: Address,
    stripeChargeId: String,
    amountPaid: Double,
    taxPaid: Double,
    treats: List[(Treat, Int)]
  ) = {
    val dateProcessed = new Date()

    val newTreatOrder = TreatOrder.create
      .treatOrderId(generateLongId)
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

    TreatOrderLineItem.createTreatOrderLineItems(treats, newTreatOrder)

    newTreatOrder
  }
}

class TreatOrderLineItem extends LongKeyedMapper[TreatOrderLineItem] with IdPK {
  def getSingleton = TreatOrderLineItem
  object orderLineItemId extends MappedLong(this){
    override def dbIndexed_? = true
    override def defaultValue = generateLongId
  }

  object order extends MappedLongForeignKey(this, TreatOrder)
  object treat extends MappedLongForeignKey(this, Treat)
  object quantity extends MappedInt(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createTreatOrderLineItems(
    treats: List[(Treat, Int)],
    order: TreatOrder
  ) = {
    treats.map { case (treat, quantity) =>
      TreatOrderLineItem.create
      .quantity(quantity)
      .treat(treat)
      .order(order)
      .saveMe
    }
  }
}

object TreatOrderLineItem extends TreatOrderLineItem with LongKeyedMetaMapper[TreatOrderLineItem]
