package com.mypetdefense.model

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.mapper._

import java.util.Date

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
  object animalType   extends MappedEnum(this, AnimalType)
  object boxType      extends MappedEnum(this, BoxType)
  object fleaTick     extends MappedLongForeignKey(this, FleaTick)
  object subscriptionItems
      extends MappedOneToMany(SubscriptionItem, SubscriptionItem.subscriptionBox)
  object addOnProducts extends MappedOneToMany(AddOnProduct, AddOnProduct.subscriptionBox)
  object basePrice     extends MappedDouble(this)
  object status extends MappedEnum(this, Status) {
    override def defaultValue: Status.Value = Status.Active
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
  object userModified extends MappedBoolean(this) {
    override def defaultValue: Boolean = false
  }
  object monthSupply extends MappedBoolean(this) {
    override def defaultValue: Boolean = true
  }
}

object SubscriptionBox extends SubscriptionBox with LongKeyedMetaMapper[SubscriptionBox] {

  def getAllUnmodifiedDogHealthWellness: List[SubscriptionBox] = {
    SubscriptionBox.findAll(
      By(SubscriptionBox.userModified, false),
      By(SubscriptionBox.animalType, AnimalType.Dog),
      By(SubscriptionBox.boxType, BoxType.healthAndWellness)
    )
  }

  def findBoxPrice(subscriptionBox: SubscriptionBox): Double = {
    (for {
      fleaTick <- subscriptionBox.fleaTick.obj
      subscription <- subscriptionBox.subscription.obj
      price <- Price.getPricesByCode(fleaTick, subscription.priceCode.get, subscriptionBox.boxType.get)
    } yield
      price.price.get
    ).openOrThrowException("Couldn't find price. Please try again.")
  }

  def createNewBox(
      subscription: Subscription,
      pet: Pet,
      upgradedBox: Boolean = false,
      monthSupply: Boolean = false,
  ): SubscriptionBox = {
    val fleaTick = FleaTick.find(By(FleaTick.size, pet.size.get))

    val newSubscriptionBox = SubscriptionBox.create
      .boxId(generateLongId)
      .subscription(subscription)
      .pet(pet)
      .animalType(pet.animalType.get)
      .fleaTick(fleaTick)
      .monthSupply(monthSupply)

    if (upgradedBox && pet.animalType == AnimalType.Dog)
      newSubscriptionBox.boxType(BoxType.healthAndWellness).saveMe()
    else
      newSubscriptionBox.boxType(BoxType.basic)

    newSubscriptionBox.basePrice(findBoxPrice(newSubscriptionBox)).saveMe()
  }
}

object BoxType extends Enumeration {
  val basic: BoxType.Value = Value("Flea & Tick Only")  // 0
  val healthAndWellness: BoxType.Value = Value("Health and Wellness") // 1
}
