package com.mypetdefense.model

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common.{Box, Full}
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
      supplements ++ Product.dentalPowderSmallForDogs.map(dentalPowder.product(_).saveMe())
    else
      supplements ++ Product.dentalPowderLargeForDogs.map(dentalPowder.product(_).saveMe())
  }

  def createNewBox(pendingPet: PendingPet): List[SubscriptionItem] = {
    val subscriptionBox = pendingPet.subscriptionBox
    val isSmallDog = pendingPet.pet.size.get == AnimalSize.DogSmallZo

    subscriptionBox.map(_.boxType.get) match {
      case Full(BoxType.healthAndWellness) =>
        val supplement = SubscriptionItem.create
          .subscriptionBox(subscriptionBox)
          .product(pendingPet.thirtyDaySupplement)
          .saveMe()

        List(supplement) ++ addDentalPowder(subscriptionBox, isSmallDog).toList
      case Full(BoxType.everydayWellness) =>
        addDentalPowder(subscriptionBox, isSmallDog).toList

      case _ => Nil
    }
  }

  def addDentalPowder(subscriptionBox: Box[SubscriptionBox], isSmallDog: Boolean) = {
    val dentalPowder = SubscriptionItem.create.subscriptionBox(subscriptionBox)
    if (isSmallDog)
      Product.dentalPowderSmallForDogs.map(dentalPowder.product(_).saveMe())
    else
      Product.dentalPowderLargeForDogs.map(dentalPowder.product(_).saveMe())
  }
}
