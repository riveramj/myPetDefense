package com.mypetdefense.model

import net.liftweb.mapper._
import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._

class Product extends LongKeyedMapper[Product] with IdPK with OneToMany[Long, Product] {
  def getSingleton = Product
  object productId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object name extends MappedString(this, 100)
  object animalType extends MappedEnum(this, AnimalType)
  object size extends MappedEnum(this, AnimalSize)
  object prices extends MappedOneToMany(Price, Price.product)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def getNameAndSize = s"${this.name}, ${this.size.toString()} pounds"

  def createProduct(
    name: String,
    animalType: AnimalType.Value,
    size: AnimalSize.Value
  ) = {
    Product.create
    .productId(generateLongId)
    .name(name)
    .animalType(animalType)
    .size(size)
    .saveMe
  }
}

object Product extends Product with LongKeyedMetaMapper[Product]
