package com.mypetdefense.model

import net.liftweb._
import mapper._
import com.mypetdefense.util.RandomIdGenerator._
import java.util.Date

import net.liftweb.common.Box

class SubscriptionBox
    extends LongKeyedMapper[SubscriptionBox]
    with IdPK
    with OneToMany[Long, SubscriptionBox] {
  def getSingleton: KeyedMetaMapper[Long, SubscriptionBox] = SubscriptionBox
  object boxId extends MappedLong(this) {
    override def dbIndexed_?        = true
    override def defaultValue: Long = generateLongId
  }

  object subscription extends MappedLongForeignKey(this, Subscription)
  object pet          extends MappedLongForeignKey(this, Pet)
  object fleaTick     extends MappedLongForeignKey(this, FleaTick)
  object subscriptionItems
      extends MappedOneToMany(SubscriptionItem, SubscriptionItem.subscriptionBox)
  object addOnProducts extends MappedOneToMany(AddOnProduct, AddOnProduct.subscriptionBox)
  object basePrice     extends MappedDouble(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def refresh: Box[SubscriptionBox] = SubscriptionBox.find(By(SubscriptionBox.boxId, boxId.get))

  def possiblePrice(subscriptionBox: SubscriptionBox): Double =
    if (subscriptionBox.subscriptionItems.toList.nonEmpty)
      subscriptionBox.pet.obj.map(basePrice).openOr(0d)
    else
      0d

  def basePrice(pet: Pet): Double = pet.size.get match {
    case AnimalSize.DogSmallZo | AnimalSize.DogMediumZo => 24.99
    case AnimalSize.DogLargeZo | AnimalSize.DogXLargeZo => 27.99
    case AnimalSize.CatAllSize                          => 12.99
  }

  def createNewBox(subscription: Subscription, pet: Pet): SubscriptionBox = {
    val fleaTick = pet.size.get match {
      case AnimalSize.DogSmallZo  => FleaTick.zoGuardSmallDog
      case AnimalSize.DogMediumZo => FleaTick.zoGuardMediumDog
      case AnimalSize.DogLargeZo  => FleaTick.zoGuardLargeDog
      case AnimalSize.DogXLargeZo => FleaTick.zoGuardXLargeDog
    }

    SubscriptionBox.create
      .boxId(generateLongId)
      .subscription(subscription)
      .pet(pet)
      .fleaTick(fleaTick)
      .basePrice(basePrice(pet))
      .saveMe()
  }

  def createBasicBox(subscription: Subscription, fleaTick: FleaTick, pet: Pet): SubscriptionBox = {
    SubscriptionBox.create
      .boxId(generateLongId)
      .subscription(subscription)
      .pet(pet)
      .fleaTick(fleaTick)
      .basePrice(basePrice(pet))
      .saveMe()
  }
}

object SubscriptionBox extends SubscriptionBox with LongKeyedMetaMapper[SubscriptionBox]
