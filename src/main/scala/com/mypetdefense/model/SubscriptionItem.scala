package com.mypetdefense.model

import java.util.Date

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
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

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
    val dentalPowder      = Product.dentalPowderForDogs
    val dentalPowderSmall = Product.dentalPowderSmallForDogs
    val dentalPowderLarge = Product.dentalPowderLargeForDogs

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
