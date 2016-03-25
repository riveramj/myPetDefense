package com.mypetdefense.model

import net.liftweb.mapper._
import java.util.Date

class Shipment extends LongKeyedMapper[Shipment] with IdPK with OneToMany[Long, Shipment] {
  def getSingleton = Shipment
  object shipmentId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object subscription extends MappedLongForeignKey(this, Subscription)
  object trackingNumber extends MappedString(this, 100)
  object dateProcessed extends MappedDateTime(this)
  object dateShipped extends MappedDateTime(this)
  object dateReceived extends MappedDateTime(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Shipment extends Shipment with LongKeyedMetaMapper[Shipment]

class ShipmentLineItem extends LongKeyedMapper[ShipmentLineItem] with IdPK {
  def getSingleton = ShipmentLineItem

  object shipment extends MappedLongForeignKey(this, Shipment)
  object product extends MappedLongForeignKey(this, Product)
}

object ShipmentLineItem extends ShipmentLineItem with LongKeyedMetaMapper[ShipmentLineItem]

