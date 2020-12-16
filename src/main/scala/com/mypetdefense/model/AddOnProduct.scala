package com.mypetdefense.model

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.mapper._

class AddOnProduct extends LongKeyedMapper[AddOnProduct] with IdPK {
  def getSingleton: KeyedMetaMapper[Long, AddOnProduct] = AddOnProduct
  object addOnProductId extends MappedLong(this) {
    override def dbIndexed_?        = true
    override def defaultValue: Long = generateLongId
  }

  object product         extends MappedLongForeignKey(this, Product)
  object subscriptionBox extends MappedLongForeignKey(this, SubscriptionBox)
  object quantity        extends MappedInt(this)
  object price           extends MappedDouble(this)
  object frequency extends MappedEnum(this, AddOnFrequency) {
    override def defaultValue: AddOnFrequency.Value = AddOnFrequency.Monthly
  }
  object addDate extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
  object removedDate extends MappedDateTime(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object AddOnProduct extends AddOnProduct with LongKeyedMetaMapper[AddOnProduct] {
  def createAddOnProduct(
      product: Product,
      subscriptionBox: SubscriptionBox,
      quantity: Int,
      price: Double
  ): AddOnProduct = {
    AddOnProduct.create
      .quantity(quantity)
      .product(product)
      .price(price)
      .subscriptionBox(subscriptionBox)
      .saveMe
  }
}

object AddOnFrequency extends Enumeration {
  val Monthly, SemiMonthly, Yearly = Value
}
