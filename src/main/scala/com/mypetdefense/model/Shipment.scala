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
  object address extends MappedString(this, 200)
  object dateProcessed extends MappedDateTime(this)
  object expectedShipDate extends MappedDateTime(this)
  object dateShipped extends MappedDateTime(this)
  object dateReceived extends MappedDateTime(this)
  object taxPaid extends MappedString(this, 100)
  object amountPaid extends MappedString(this, 100)
  object shipmentLineItems extends MappedOneToMany(ShipmentLineItem, ShipmentLineItem.shipment)
  object status extends MappedEnum(this, Status) {
    override def defaultValue = Status.Active
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def cancel = {
    this
      .status(Status.Cancelled)
      .saveMe
  }

  def refresh = Shipment.find(By(Shipment.shipmentId, shipmentId.get))
}

object Shipment extends Shipment with LongKeyedMetaMapper[Shipment] {
  def createShipment(user: User, stripePaymentId: String, amountPaid: String, taxPaid: String) = {
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
      .amountPaid(amountPaid)
      .taxPaid(taxPaid)
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
  object petName extends MappedString(this, 100)
  object pet extends MappedLongForeignKey(this, Pet)

  def getShipmentItem = {
    val productNameSize = this.product.obj.map(_.getNameAndSize).openOr("")
    s"${this.petName.get} - ${productNameSize}".replace("null", "")
  }
}

object ShipmentLineItem extends ShipmentLineItem with LongKeyedMetaMapper[ShipmentLineItem] {
  def createShipmentItems(shipment: Shipment, user: User) = {
    val pets = Pet.findAll(By(Pet.user, user), By(Pet.status, Status.Active))
    val products = pets.map(_.product.obj)

    for {
      pet <- pets
      product <- pet.product.obj
    } yield {
      ShipmentLineItem.create
        .shipmentLineItemId(generateLongId)
        .shipment(shipment)
        .product(product)
        .pet(pet)
        .petName(pet.name.get)
        .saveMe
    }
  }
}

