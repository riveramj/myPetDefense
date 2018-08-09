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
  object sizeName extends MappedString(this, 100)
  object prices extends MappedOneToMany(Price, Price.product)
  object reviews extends MappedOneToMany(Review, Review.product)
  object rating extends MappedDouble(this)
  object reviewCount extends MappedInt(this)
  object imageName extends MappedString(this, 100)
  object sku extends MappedString(this, 100)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def getNameAndSize = s"${this.name}, ${this.size.toString()} lbs"
  
  def getSizeAndSizeName = s"${this.sizeName}, ${this.size.toString()} lbs"

  def createProduct(
    name: String,
    animalType: AnimalType.Value,
    size: AnimalSize.Value,
    sizeName: String,
    imageName: String
  ) = {
    Product.create
    .productId(generateLongId)
    .name(name)
    .animalType(animalType)
    .size(size)
    .sizeName(sizeName)
    .imageName(imageName)
    .saveMe
  }

  def isZoGuard_? = this.name.get.toLowerCase.contains("zoguard")
}

object Product extends Product with LongKeyedMetaMapper[Product]
