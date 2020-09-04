package com.mypetdefense.model

import net.liftweb._
import common._
import mapper._
import com.mypetdefense.util.RandomIdGenerator._
import java.util.Date

class Shipment extends LongKeyedMapper[Shipment] with IdPK with OneToMany[Long, Shipment] {
  def getSingleton: KeyedMetaMapper[Long, Shipment] = Shipment
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
  object freeUpgradeSample extends MappedBoolean(this) {
    override def defaultValue = false
  }
  object status extends MappedEnum(this, Status) {
    override def defaultValue: Status.Value = Status.Active
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def refresh: Box[Shipment] = Shipment.find(By(Shipment.shipmentId, shipmentId.get))
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
    shipmentStatus: ShipmentStatus.Value,
    sendFreeUpgrade: Boolean = false
  ): Shipment = {
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
      .freeUpgradeSample(sendFreeUpgrade)
      .saveMe

    ShipmentLineItem.createShipmentItems(shipment, user, inserts, sendFreeUpgrade)

    shipment
  }
}

class ShipmentLineItem extends LongKeyedMapper[ShipmentLineItem] with IdPK {
  def getSingleton: KeyedMetaMapper[Long, ShipmentLineItem] = ShipmentLineItem
  object shipmentLineItemId extends MappedLong(this){
    override def dbIndexed_? = true
    override def defaultValue: Long = generateLongId
  }
  object shipment extends MappedLongForeignKey(this, Shipment)
  object fleaTick extends MappedLongForeignKey(this, FleaTick)
  object product extends MappedLongForeignKey(this, Product)
  object petName extends MappedString(this, 100)
  object pet extends MappedLongForeignKey(this, Pet)
  object insert extends MappedLongForeignKey(this, Insert)

  def getFleaTickPetNameItemSize: String = {
    val fleaTickNameSize = this.fleaTick.obj.map(_.getNameAndSize).openOr("")
    s"${this.petName.get} - $fleaTickNameSize".replace("null", "")
  }

  def sendFreeUpgradeItems(shipment: Shipment, pet: Pet) : List[ShipmentLineItem] = {
    val products = List(Product.hipAndJoint, Product.calming, Product.multiVitamin, Product.dentalPowder).flatten

    products.map { item =>
      ShipmentLineItem.create
        .shipmentLineItemId(generateLongId)
        .shipment(shipment)
        .product(item)
        .pet(pet)
        .petName(pet.name.get)
        .saveMe
    }
  }

  def createShipmentItems(shipment: Shipment, user: User, inserts: List[Insert], sendFreeUpgrade: Boolean): List[ShipmentLineItem] = {
    for {
      subscription <- user.subscription.toList
      box <- subscription.subscriptionBoxes
      pet <- box.pet.obj
      fleaTick = box.fleaTick.obj
      subscriptionItems = box.subscriptionItems
    } yield {
      ShipmentLineItem.create
        .shipmentLineItemId(generateLongId)
        .shipment(shipment)
        .fleaTick(fleaTick)
        .pet(pet)
        .petName(pet.name.get)
        .saveMe


      if (sendFreeUpgrade && pet.animalType.get == AnimalType.Dog)
        sendFreeUpgradeItems(shipment, pet)

      subscriptionItems.map { item =>
        ShipmentLineItem.create
          .shipmentLineItemId(generateLongId)
          .shipment(shipment)
          .product(item.product.obj)
          .pet(pet)
          .petName(pet.name.get)
          .saveMe
      }

      inserts.map { insert =>
        ShipmentLineItem.create
          .shipmentLineItemId(generateLongId)
          .shipment(shipment)
          .insert(insert)
          .saveMe
      }
    }
  }.flatten
}

object ShipmentLineItem extends ShipmentLineItem with LongKeyedMetaMapper[ShipmentLineItem]

object ShipmentStatus extends Enumeration {
  val Paid: ShipmentStatus.Value = Value
  val LabelCreated: ShipmentStatus.Value = Value("Label Created")
  val InTransit: ShipmentStatus.Value = Value("In Transit")
  val Delivered: ShipmentStatus.Value = Value
  val DelayedDelivery: ShipmentStatus.Value = Value("Delayed Delivery")
  val Refused: ShipmentStatus.Value = Value
  val FailedDelivery: ShipmentStatus.Value = Value("Failed Delivery")
  val Other: ShipmentStatus.Value = Value
}
