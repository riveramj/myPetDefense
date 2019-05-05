package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class Product extends LongKeyedMapper[Product] with IdPK with OneToMany[Long, Product] {
  def getSingleton = Product
  object productId extends MappedLong(this) {
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

  def createNewProduct(name: String, price: Double, weight: Double, sku: String) = {
    Product.create
    .name(name)
    .price(price)
    .weight(weight)
    .sku(sku)
    .saveMe
  }
}

object Product extends Product with LongKeyedMetaMapper[Product]
