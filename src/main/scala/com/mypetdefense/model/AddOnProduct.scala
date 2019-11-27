package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class AddOnProduct extends LongKeyedMapper[AddOnProduct] with IdPK {
  def getSingleton = AddOnProduct
  object addOnProductId extends MappedLong(this){
    override def dbIndexed_? = true
    override def defaultValue = generateLongId
  }

  object product extends MappedLongForeignKey(this, Product)
  object subscription extends MappedLongForeignKey(this, Subscription)
  object quantity extends MappedInt(this)
  object price extends MappedDouble(this)
  object frequency extends MappedEnum(this, AddOnFrequency) {
    override def defaultValue = AddOnFrequency.Monthly
  }
  object addDate extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
  object removedDate extends MappedDateTime(this)
  object status extends MappedEnum(this, AddOnStatus) {
    override def defaultValue = AddOnStatus.Active
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createAddOnProduct(
    product: Product,
    subscription: Subscription,
    quantity: Int,
    price: Double
  ) = {
    AddOnProduct.create
      .quantity(quantity)
      .product(product)
      .price(price)
      .subscription(subscription)
      .saveMe
  }
}

object AddOnProduct extends AddOnProduct with LongKeyedMetaMapper[AddOnProduct]

object AddOnStatus extends Enumeration {
  val Active, Removed = Value
}

object AddOnFrequency extends Enumeration {
  val Monthly, SemiMonthly, Yearly = Value
}
