package com.mypetdefense.model

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.mapper._

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

  def createFirstBox(subscriptionBox: SubscriptionBox, firstBox: Boolean = true): List[SubscriptionItem] = {
    val products = {
      if(firstBox)
        ProductSchedule.getFirstBoxProducts
      else
        ProductSchedule.getRegularScheduleBoxProducts
    }

    val smallSizes = List(AnimalSize.DogSmallZo, AnimalSize.DogSmallShld, AnimalSize.DogSmallAdv)
    val isSmallDog = subscriptionBox.fleaTick.obj.map(_.size.get).forall(smallSizes.contains)

    val supplements = products.map { product =>
      val newItem = SubscriptionItem.create.subscriptionBox(subscriptionBox)
      newItem.product(product).saveMe()
    }

    val dentalPowder = SubscriptionItem.create.subscriptionBox(subscriptionBox)
    if (isSmallDog)
      supplements ++ Product.dentalPowderSmall.map(dentalPowder.product(_).saveMe())
    else
      supplements ++ Product.dentalPowderLarge.map(dentalPowder.product(_).saveMe())
  }
}
