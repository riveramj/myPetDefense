package com.mypetdefense.model

import net.liftweb._
import mapper._

import com.mypetdefense.util.RandomIdGenerator._
import java.util.Date

class SubscriptionBox extends LongKeyedMapper[SubscriptionBox] with IdPK with OneToMany[Long, SubscriptionBox] {
  def getSingleton = SubscriptionBox
  object boxId extends MappedLong(this){
    override def dbIndexed_? = true
    override def defaultValue = generateLongId
  }

  object subscription extends MappedLongForeignKey(this, Subscription)
  object pet extends MappedLongForeignKey(this, Pet)
  object fleaTick extends MappedLongForeignKey(this, FleaTick)
  object subscriptionItems extends MappedOneToMany(SubscriptionItem, SubscriptionItem.subscriptionBox)
  object addOnProducts extends MappedOneToMany(AddOnProduct, AddOnProduct.subscriptionBox)
  object basePrice extends MappedDouble(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewBox(subscription: Subscription, pet: Pet) = {
    val fleaTick = pet.size.get match {
      case AnimalSize.DogSmallZo => FleaTick.zoGuardSmallDog
      case AnimalSize.DogMediumZo => FleaTick.zoGuardMediumDog
      case AnimalSize.DogLargeZo => FleaTick.zoGuardLargeDog
      case AnimalSize.DogXLargeZo => FleaTick.zoGuardXLargeDog
    }

    val basePrice = pet.size.get match {
      case AnimalSize.DogSmallZo | AnimalSize.DogMediumZo => 24.99
      case AnimalSize.DogLargeZo | AnimalSize.DogXLargeZo => 27.99
    }

    SubscriptionBox.create
      .boxId(generateLongId)
      .subscription(subscription)
      .pet(pet)
      .fleaTick(fleaTick)
      .basePrice(basePrice)
      .saveMe()
  }
}

object SubscriptionBox extends SubscriptionBox with LongKeyedMetaMapper[SubscriptionBox]