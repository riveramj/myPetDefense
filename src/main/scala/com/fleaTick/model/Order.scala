package com.fleaTick.model

import net.liftweb._
  import mapper._
  import common._
  import util._
  
import java.util.Date

import com.fleaTick.util.RandomIdGenerator._

class Order extends LongKeyedMapper[Order] with IdPK with OneToMany[Long, Order] {
  def getSingleton = Order
  object orderId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object parent extends MappedLongForeignKey(this, Parent)
  object subscription extends MappedLongForeignKey(this, Subscription)
  object products extends MappedOneToMany(OrderLineItem, OrderLineItem.order)
  object total extends MappedString(this, 100)
  object orderDate extends MappedDateTime(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
  
  def createNewOrder(
    parent: Parent,
    subscription: Subscription,
    products: List[Product],
    total: String
  ) = {
    Order.create
    .orderId(generateLongId)
    .parent(parent)
    .subscription(subscription)
    .total(total)
    .orderDate(new Date())
    .saveMe
  }
}

object Order extends Order with LongKeyedMetaMapper[Order]

class OrderLineItem extends LongKeyedMapper[OrderLineItem] with IdPK {
  def getSingleton = OrderLineItem

  object order extends MappedLongForeignKey(this, Order)
  object product extends MappedLongForeignKey(this, Product)
}

object OrderLineItem extends OrderLineItem with LongKeyedMetaMapper[OrderLineItem]

