package com.fleaTick.model

import net.liftweb.mapper._
import java.util.Date

import com.fleaTick.util.RandomIdGenerator._

class Product extends LongKeyedMapper[Product] with IdPK with OneToMany[Long, Product] {
  def getSingleton = Product
  object productId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object name extends MappedString(this, 100)
  object description extends MappedString(this, 100)
  object price extends MappedLong(this)
  object animalType extends MappedEnum(this, AnimalType)
  object size extends MappedEnum(this, AnimalSize)
  object subscriptionType extends MappedEnum(this, SubscriptionType)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createProduct(
    name: String,
    description: String,
    price: Long,
    animalType: AnimalType.Value,
    animalSize: AnimalSize.Value,
    subscriptionType: SubscriptionType.Value
  ) = {
    Product.create
    .productId(generateLongId)
    .name(name)
    .description(description)
    .price(price)
    .animalType(animalType)
    .size(animalSize)
    .subscriptionType(subscriptionType)
    .saveMe
  }


}

object Product extends Product with LongKeyedMetaMapper[Product]
