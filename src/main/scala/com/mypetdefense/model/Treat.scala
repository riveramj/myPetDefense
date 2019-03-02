package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class Treat extends LongKeyedMapper[Treat] with IdPK with OneToMany[Long, Treat] {
  def getSingleton = Treat
  object treatId extends MappedLong(this) {
    override def dbIndexed_? = true
    override def defaultValue = generateLongId
  }
  object name extends MappedString(this, 100)
  object price extends MappedDouble(this)
  object sku extends MappedString(this, 100)
  object weight extends MappedDouble(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewTreat(name: String, price: Double, weight: Double, sku: String) = {
    Treat.create
    .name(name)
    .price(price)
    .weight(weight)
    .sku(sku)
    .saveMe
  }
}

object Treat extends Treat with LongKeyedMetaMapper[Treat]
