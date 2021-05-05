package com.mypetdefense.model

import java.time.{LocalDate, ZoneId}
import java.util.Date

import com.mypetdefense.util.DateHelper.{monthDayOne, nowDate}
import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.Helpers.tryo

class Shipment extends LongKeyedMapper[Shipment] with IdPK with OneToMany[Long, Shipment] {
  def getSingleton: KeyedMetaMapper[Long, Shipment] = Shipment
  object shipmentId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object subscription       extends MappedLongForeignKey(this, Subscription)
  object shipStationOrderId extends MappedInt(this)
  object stripePaymentId    extends MappedString(this, 100)
  object stripeChargeId     extends MappedString(this, 100)
  object trackingNumber     extends MappedString(this, 100)
  object address            extends MappedString(this, 200)
  object dateProcessed      extends MappedDateTime(this)
  object dateRefunded       extends MappedDateTime(this)
  object expectedShipDate   extends MappedDateTime(this)
  object dateShipped        extends MappedDateTime(this)
  object dateReceived       extends MappedDateTime(this)
  object taxPaid            extends MappedString(this, 100)
  object amountPaid         extends MappedString(this, 100)
  object shipmentLineItems  extends MappedOneToMany(ShipmentLineItem, ShipmentLineItem.shipment)
  object insert             extends MappedString(this, 100)
  object shipmentStatus     extends MappedEnum(this, ShipmentStatus)
  object deliveryNotes      extends MappedString(this, 100)
  object freeUpgradeSample extends MappedBoolean(this) {
    override def defaultValue = false
  }
  object status extends MappedEnum(this, Status) {
    override def defaultValue: Status.Value = Status.Active
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def getProcessDateOfShipment: LocalDate =
    this.dateProcessed.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate

  def getMailedDateOfShipment: Box[LocalDate] =
    tryo(this.dateShipped.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate)

  def actualShipmentLineItems: List[ShipmentLineItem] =
    this.reload.shipmentLineItems.toList

  def shipmentLineItemsByPets: Map[Pet, List[ShipmentLineItem]] =
    this.shipmentLineItems.toList
      .groupBy(_.pet.obj)
      .collect { case (Full(pet), items) => pet -> items }
}

object Shipment extends Shipment with LongKeyedMetaMapper[Shipment] {
  def findMtdShipments: List[Shipment] = {
    Shipment.findAll(
      By_>=(Shipment.createdAt, monthDayOne)
    )
  }

  def findTodayShipments: List[Shipment] = {
    Shipment.findAll(
      By_>=(Shipment.dateProcessed, nowDate)
    )
  }

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

  def notCancelledWithoutTrackingNumber(createdUntil: Date): List[Shipment] =
    Shipment
      .findAll(
        NotBy(Shipment.status, Status.Cancelled),
        By_<=(Shipment.dateProcessed, createdUntil),
        By(Shipment.trackingNumber, "") // "" default value
      )

  def notCancelledWithEmptyLineItems(createdUntil: Date): List[Shipment] = {
    Shipment
      .findAll(
        ByList(Shipment.shipmentStatus, List(ShipmentStatus.LabelCreated, ShipmentStatus.Paid)),
        By_<=(Shipment.dateProcessed, createdUntil)
      )
      .filter(_.shipmentLineItems.isEmpty)
  }
}

class ShipmentLineItem extends LongKeyedMapper[ShipmentLineItem] with IdPK {
  def getSingleton: KeyedMetaMapper[Long, ShipmentLineItem] = ShipmentLineItem
  object shipmentLineItemId extends MappedLong(this) {
    override def dbIndexed_?        = true
    override def defaultValue: Long = generateLongId
  }
  object shipment extends MappedLongForeignKey(this, Shipment)
  object fleaTick extends MappedLongForeignKey(this, FleaTick)
  object product  extends MappedLongForeignKey(this, Product)
  object petName  extends MappedString(this, 100)
  object pet      extends MappedLongForeignKey(this, Pet)
  object insert   extends MappedLongForeignKey(this, Insert)

  def getPetNameProductName: String = {
    val productName = this match {
      case _ if this.fleaTick.obj.isDefined => this.fleaTick.obj.map(_.getNameAndSize).openOr("")
      case _ if this.product.obj.isDefined  => this.product.obj.map(_.name.get).openOr("")
      case _ if this.insert.obj.isDefined   => this.insert.obj.map(_.name.get).openOr("")
      case _                                => ""
    }

    s"${this.petName.get} - $productName"
  }

  def sendFreeUpgradeItems(shipment: Shipment, pet: Pet): List[ShipmentLineItem] = {
    val smallDogs = List(AnimalSize.DogSmallAdv, AnimalSize.DogSmallShld, AnimalSize.DogSmallZo)

    val products = ProductSchedule.getFirstBoxProducts ++ {
      if (smallDogs.contains(pet.size.get))
        Product.dentalPowderSmallForDogs.toList
      else
        Product.dentalPowderLargeForDogs.toList
    }

    shipment.reload.freeUpgradeSample(true).saveMe()

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

  def createShipmentItems(
      shipment: Shipment,
      user: User,
      inserts: List[Insert],
      sendFreeUpgrade: Boolean
  ): List[ShipmentLineItem] = {
    for {
      subscription <- user.subscription.toList
      box          <- subscription.subscriptionBoxes
      if box.status.get == Status.Active
      pet          <- box.pet.obj
      if pet.status.get == Status.Active
      fleaTick          = box.fleaTick.obj
      subscriptionItems = box.subscriptionItems
    } yield {
      ShipmentLineItem.create
        .shipmentLineItemId(generateLongId)
        .shipment(shipment)
        .fleaTick(fleaTick)
        .pet(pet)
        .petName(pet.name.get)
        .saveMe

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
  val Paid: ShipmentStatus.Value            = Value
  val LabelCreated: ShipmentStatus.Value    = Value("Label Created")
  val InTransit: ShipmentStatus.Value       = Value("In Transit")
  val Delivered: ShipmentStatus.Value       = Value
  val DelayedDelivery: ShipmentStatus.Value = Value("Delayed Delivery")
  val Refused: ShipmentStatus.Value         = Value
  val FailedDelivery: ShipmentStatus.Value  = Value("Failed Delivery")
  val Other: ShipmentStatus.Value           = Value
}
