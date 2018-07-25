package com.mypetdefense.model

import net.liftweb.mapper._
import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._
import com.mypetdefense.snippet.NewAddress

class FriendsFamilyOrder extends LongKeyedMapper[FriendsFamilyOrder] with IdPK with OneToMany[Long, FriendsFamilyOrder] {
  def getSingleton = FriendsFamilyOrder
  object orderId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object name extends MappedString(this, 100)
  object email extends MappedString(this, 100)
  object stripeOrderId extends MappedString(this, 100)
  object total extends MappedDouble(this)
  object tax extends MappedDouble(this)
  object donation extends MappedDouble(this)
  object street1 extends MappedString(this, 100)
  object street2 extends MappedString(this, 100)
  object city extends MappedString(this, 100)
  object state extends MappedString(this, 100)
  object zip extends MappedString(this, 100)
  object products extends MappedOneToMany(FriendsFamilyOrderLineItem, FriendsFamilyOrderLineItem.order)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createOrder(
    name: String,
    email: String,
    stripeOrderId: String,
    address: NewAddress,
    total: Double,
    tax: Double,
    products: List[(Int, FriendsFamilyProduct)]
  ) = {
    val order = FriendsFamilyOrder.create
      .orderId(generateLongId)
      .name(name)
      .email(email)
      .stripeOrderId(stripeOrderId)
      .street1(address.street1)
      .street2(address.street2.getOrElse(""))
      .city(address.city)
      .state(address.state)
      .zip(address.zip)
      .total(total)
      .tax(tax)
      .saveMe

      FriendsFamilyOrderLineItem.createFriendsFamilyOrderLineItems(products, order)

      order
  }
}

object FriendsFamilyOrder extends FriendsFamilyOrder with LongKeyedMetaMapper[FriendsFamilyOrder]

class FriendsFamilyOrderLineItem extends LongKeyedMapper[FriendsFamilyOrderLineItem] with IdPK {
  def getSingleton = FriendsFamilyOrderLineItem
  object orderLineItemId extends MappedLong(this){
    override def dbIndexed_? = true
  }

  object order extends MappedLongForeignKey(this, FriendsFamilyOrder)
  object product extends MappedLongForeignKey(this, FriendsFamilyProduct)
  object quantity extends MappedInt(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object FriendsFamilyOrderLineItem extends FriendsFamilyOrderLineItem with LongKeyedMetaMapper[FriendsFamilyOrderLineItem] {
  
  def createFriendsFamilyOrderLineItems(
    products: List[(Int, FriendsFamilyProduct)],
    order: FriendsFamilyOrder
  ) = {

    products.map { product =>
      FriendsFamilyOrderLineItem.create
      .orderLineItemId(generateLongId)
      .quantity(product._1)
      .product(product._2)
      .order(order)
      .saveMe
    }
  }
}
