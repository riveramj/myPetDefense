package com.mypetdefense.model

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common.Full
import net.liftweb.mapper._

class SubscriptionItem extends LongKeyedMapper[SubscriptionItem] with IdPK {
  def getSingleton: KeyedMetaMapper[Long, SubscriptionItem] = SubscriptionItem

  object subscriptionItemId extends MappedLong(this) {
    override def dbIndexed_?        = true
    override def defaultValue: Long = generateLongId
  }
  object product         extends MappedLongForeignKey(this, Product)
  object subscriptionBox extends MappedLongForeignKey(this, SubscriptionBox)
  object createdAt       extends MappedZonedDateTime(this, useNowAsDefault = true)
}

object SubscriptionItem extends SubscriptionItem with LongKeyedMetaMapper[SubscriptionItem] {
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
    val products          = ProductSchedule.getFirstBoxProducts
    val smallSizes        = List(AnimalSize.DogSmallZo, AnimalSize.DogSmallShld, AnimalSize.DogSmallAdv)
    val isSmallDog        = subscriptionBox.fleaTick.obj.map(_.size.get).forall(smallSizes.contains)
    val dentalPowder      = Product.dentalPowder
    val dentalPowderSmall = Product.dentalPowderSmall
    val dentalPowderLarge = Product.dentalPowderLarge

    products.flatMap { product =>
      val newItem = SubscriptionItem.create.subscriptionBox(subscriptionBox)

      if (!dentalPowder.contains(product))
        Full(newItem.product(product).saveMe())
      else if (isSmallDog)
        dentalPowderSmall.map(newItem.product(_).saveMe())
      else
        dentalPowderLarge.map(newItem.product(_).saveMe())
    }
  }
}
