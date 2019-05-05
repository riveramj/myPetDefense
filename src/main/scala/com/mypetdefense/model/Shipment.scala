package com.mypetdefense.model

import net.liftweb._
  import common._
  import mapper._

import com.mypetdefense.util.RandomIdGenerator._
import java.util.Date

class Shipment extends LongKeyedMapper[Shipment] with IdPK with OneToMany[Long, Shipment] {
  def getSingleton = Shipment
  object shipmentId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object subscription extends MappedLongForeignKey(this, Subscription)
  object shipStationOrderId extends MappedInt(this)
  object stripePaymentId extends MappedString(this, 100)
  object stripeChargeId extends MappedString(this, 100)
  object trackingNumber extends MappedString(this, 100)
  object address extends MappedString(this, 200)
  object dateProcessed extends MappedDateTime(this)
  object dateRefunded extends MappedDateTime(this)
  object expectedShipDate extends MappedDateTime(this)
  object dateShipped extends MappedDateTime(this)
  object dateReceived extends MappedDateTime(this)
  object taxPaid extends MappedString(this, 100)
  object amountPaid extends MappedString(this, 100)
  object shipmentLineItems extends MappedOneToMany(ShipmentLineItem, ShipmentLineItem.shipment)
  object insert extends MappedString(this, 100)
  object shipmentStatus extends MappedEnum(this, ShipmentStatus)
  object deliveryNotes extends MappedString(this, 100)
  object status extends MappedEnum(this, Status) {
    override def defaultValue = Status.Active
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def refresh = Shipment.find(By(Shipment.shipmentId, shipmentId.get))
}

object Shipment extends Shipment with LongKeyedMetaMapper[Shipment] {
  def createShipment(
    user: User,
    subscription: Subscription,
    stripePaymentId: String,
    stripeChargeId: Box[String],
    amountPaid: String,
    taxPaid: String,
    inserts: List[Insert],
    shipmentStatus: ShipmentStatus.Value
  ) = {
    val dateProcessed = new Date()

    val shipment = Shipment.create
      .shipmentId(generateLongId)
      .stripePaymentId(stripePaymentId)
      .stripeChargeId(stripeChargeId.openOr(""))
      .subscription(subscription)
      .expectedShipDate(subscription.nextShipDate.get)
      .dateProcessed(dateProcessed)
      .amountPaid(amountPaid)
      .taxPaid(taxPaid)
      .shipmentStatus(shipmentStatus)
      .saveMe

    ShipmentLineItem.createShipmentItems(shipment, user, inserts)

    shipment
  }
}

class ShipmentLineItem extends LongKeyedMapper[ShipmentLineItem] with IdPK {
  def getSingleton = ShipmentLineItem
  object shipmentLineItemId extends MappedLong(this){
    override def dbIndexed_? = true
    override def defaultValue = generateLongId
  }
  object shipment extends MappedLongForeignKey(this, Shipment)
  object fleaTick extends MappedLongForeignKey(this, FleaTick)
  object petName extends MappedString(this, 100)
  object pet extends MappedLongForeignKey(this, Pet)
  object insert extends MappedLongForeignKey(this, Insert)

  def getFleaTickPetNameItemSize = {
    val fleaTickNameSize = this.fleaTick.obj.map(_.getNameAndSize).openOr("")
    s"${this.petName.get} - ${fleaTickNameSize}".replace("null", "")
  }

  def createShipmentItems(shipment: Shipment, user: User, inserts: List[Insert]) = {
    val pets = Pet.findAll(By(Pet.user, user), By(Pet.status, Status.Active))
    val products = pets.map(_.fleaTick.obj)

    for {
      pet <- pets
      product <- pet.fleaTick.obj
    } yield {
      ShipmentLineItem.create
        .shipmentLineItemId(generateLongId)
        .shipment(shipment)
        .fleaTick(product)
        .pet(pet)
        .petName(pet.name.get)
        .saveMe
    }

    inserts.map { insert =>
      ShipmentLineItem.create
        .shipment(shipment)
        .insert(insert)
        .saveMe
    }
  }
}

object ShipmentLineItem extends ShipmentLineItem with LongKeyedMetaMapper[ShipmentLineItem]

object ShipmentStatus extends Enumeration {
  val Paid = Value
  val LabelCreated = Value("Label Created")
  val InTransit = Value("In Transit")
  val Delivered = Value
  val DelayedDelivery = Value("Delayed Delivery")
  val Refused = Value
  val FailedDelivery = Value("Failed Delivery")
  val Other = Value
}
