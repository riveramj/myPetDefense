package com.fleaTick.model

import net.liftweb.mapper._
import java.util.Date

class Shipment extends LongKeyedMapper[Shipment] with IdPK with OneToMany[Long, Shipment] {
  def getSingleton = Shipment
  object ShipmentId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object order extends MappedLongForeignKey(this, Order)
  object trackingNumber extends MappedString(this, 100)
  object dateProcessed extends MappedDateTime(this)
  object dateShipped extends MappedDateTime(this)
  object dateReceived extends MappedDateTime(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Shipment extends Shipment with LongKeyedMetaMapper[Shipment]

