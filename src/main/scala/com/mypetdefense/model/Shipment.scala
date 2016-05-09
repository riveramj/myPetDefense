package com.mypetdefense.model

import net.liftweb.mapper._

import com.mypetdefense.util.RandomIdGenerator._
import java.util.Date

class Shipment extends LongKeyedMapper[Shipment] with IdPK with OneToMany[Long, Shipment] {
  def getSingleton = Shipment
  object shipmentId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object subscription extends MappedLongForeignKey(this, Subscription)
  object stripePaymentId extends MappedString(this, 100)
  object trackingNumber extends MappedString(this, 100)
  object dateProcessed extends MappedDateTime(this)
  object expectedShipDate extends MappedDateTime(this)
  object dateShipped extends MappedDateTime(this)
  object dateReceived extends MappedDateTime(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Shipment extends Shipment with LongKeyedMetaMapper[Shipment] {
  def createShipment(user: User, stripePaymentId: String) = {
    (for {
      subscription <- Subscription.find(By(Subscription.user, user))
      dateProcessed = new Date()
    } yield {
      val shipment = Shipment.create
      .shipmentId(generateLongId)
      .stripePaymentId(stripePaymentId)
      .subscription(subscription)
      .expectedShipDate(subscription.nextShipDate.get)
      .dateProcessed(dateProcessed)
      .saveMe

      ShipmentLineItem.createShipmentItems(shipment, user)

      shipment
    })
  }
}

class ShipmentLineItem extends LongKeyedMapper[ShipmentLineItem] with IdPK {
  def getSingleton = ShipmentLineItem
  object shipmentLineItemId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object shipment extends MappedLongForeignKey(this, Shipment)
  object product extends MappedLongForeignKey(this, Product)
}

object ShipmentLineItem extends ShipmentLineItem with LongKeyedMetaMapper[ShipmentLineItem] {
  def createShipmentItems(shipment: Shipment, user: User) = {
    val pets = Pet.findAll(By(Pet.user, user))
    val products = pets.map(_.product.obj)

    products.map { product =>
      ShipmentLineItem.create
      .shipmentLineItemId(generateLongId)
      .shipment(shipment)
      .product(product)
      .saveMe
    }
  }
}

