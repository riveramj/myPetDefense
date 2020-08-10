package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class Product extends LongKeyedMapper[Product] with IdPK with OneToMany[Long, Product] {
  def getSingleton: KeyedMetaMapper[Long, Product] = Product
  object productId extends MappedLong(this) {
    override def dbIndexed_? = true
    override def defaultValue: Long = generateLongId
  }
  object name extends MappedString(this, 100)
  object price extends MappedDouble(this)
  object sku extends MappedString(this, 100)
  object weight extends MappedDouble(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewProduct(name: String, sku: String): Product = {
    Product.create
    .name(name)
    .sku(sku)
    .saveMe
  }

  def hipAndJoint: Box[Product] = Product.find(By(Product.name, "Hip & Joint Chews"))
  def calming: Box[Product] = Product.find(By(Product.name, "Calming Chews"))
  def multiVitamin: Box[Product] = Product.find(By(Product.name, "Multi-Vitamin Chews"))
  def dentalPowder: Box[Product] = Product.find(By(Product.name, "Dental Powder"))
}

object Product extends Product with LongKeyedMetaMapper[Product]
