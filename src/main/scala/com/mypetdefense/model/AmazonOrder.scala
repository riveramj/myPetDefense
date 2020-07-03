package com.mypetdefense.model 

import net.liftweb._
  import mapper._
  import common.Box

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._

class AmazonOrder extends LongKeyedMapper[AmazonOrder] with IdPK {
  def getSingleton = AmazonOrder
  object orderId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object amazonOrderId extends MappedString(this, 100)
  object email extends MappedString(this, 200)
  object phone extends MappedString(this, 200)
  object sku extends MappedString(this, 200)
  object quantityPurchased extends MappedInt(this)
  object productPrice extends MappedDouble(this)
  object productDiscount extends MappedDouble(this)
  object carrier extends MappedString(this, 100)
  object productName extends MappedString(this, 200)
  object name extends MappedString(this, 100)
  object address1 extends MappedString(this, 200)
  object address2 extends MappedString(this, 100)
  object address3 extends MappedString(this, 100)
  object city extends MappedString(this, 100)
  object state extends MappedString(this, 100)
  object zip extends MappedString(this, 100)
  object purchaseDate extends MappedDateTime(this)
  object animalType extends MappedEnum(this, AnimalType)
  object product extends MappedEnum(this, AmazonProduct)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createAmazonOrder(
    amazonOrderId: String,
    email: String,
    phone: String,
    quantityPurchased: Int,
    productPrice: Double,
    productDiscount: Double,
    carrier: String,
    productName: String,
    sku: String,
    name: String,
    address1: String,
    address2: String,
    address3: String,
    city: String,
    state: String,
    zip: String,
    purchaseDate: Date
  ) = {
    AmazonOrder.create
    .amazonOrderId(amazonOrderId)
    .email(email)
    .phone(phone)
    .quantityPurchased(quantityPurchased)
    .productPrice(productPrice)
    .productDiscount(productDiscount)
    .carrier(carrier)
    .productName(productName)
    .orderId(generateLongId)
    .sku(sku)
    .name(name)
    .address1(address1)
    .address2(address2)
    .address3(address3)
    .city(city)
    .state(state)
    .zip(zip)
    .purchaseDate(purchaseDate)
    .saveMe
  }
}

object AmazonOrder extends AmazonOrder with LongKeyedMetaMapper[AmazonOrder]

object AmazonProduct extends Enumeration {
  val ZoGuardPlus, AdventurePlus, ShieldTecPlus, Salvo = Value
}

