package com.fleaTick.model

import net.liftweb.mapper._
import java.util.Date

class Product extends LongKeyedMapper[Product] with IdPK with OneToMany[Long, Product] {
  def getSingleton = Product
  object ProductId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object name extends MappedString(this, 100)
  object description extends MappedString(this, 100)
  object price extends MappedString(this, 100)
  object animalType extends MappedEnum(this, AnimalType)
  object size extends MappedEnum(this, AnimalSize)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Product extends Product with LongKeyedMetaMapper[Product]

object AnimalType extends Enumeration {
  val Dog, Cat = Value
}

object AnimalSize extends Enumeration {
  val Small, Medium, Large, XLarge = Value
}
