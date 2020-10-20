package com.mypetdefense.model

import net.liftweb._
import mapper._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class SubscriptionItem extends LongKeyedMapper[SubscriptionItem] with IdPK {
  def getSingleton: KeyedMetaMapper[Long, SubscriptionItem] = SubscriptionItem
  object subscriptionItemId extends MappedLong(this) {
    override def dbIndexed_?        = true
    override def defaultValue: Long = generateLongId
  }

  object product         extends MappedLongForeignKey(this, Product)
  object subscriptionBox extends MappedLongForeignKey(this, SubscriptionBox)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createSubscriptionItem(
      product: Product,
      subscriptionBox: SubscriptionBox
  ): SubscriptionItem = {
    SubscriptionItem.create
      .product(product)
      .subscriptionBox(subscriptionBox)
      .saveMe
  }

  def createFirstBox(subscriptionBox: SubscriptionBox): List[SubscriptionItem] = {
    val products = List(
      Product.skinAndCoat,
      Product.multiVitamin,
      Product.probiotic,
      Product.dentalPowder
    ).flatten
    products.map { product =>
      SubscriptionItem.create
        .product(product)
        .subscriptionBox(subscriptionBox)
        .saveMe
    }
  }
}

object SubscriptionItem extends SubscriptionItem with LongKeyedMetaMapper[SubscriptionItem]
