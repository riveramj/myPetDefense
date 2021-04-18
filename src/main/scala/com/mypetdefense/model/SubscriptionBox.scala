package com.mypetdefense.model

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.mapper._

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
}

object SubscriptionBox extends SubscriptionBox with LongKeyedMetaMapper[SubscriptionBox] {

  def getAllUnmodifiedDogHealthWellness: List[SubscriptionBox] = {
    SubscriptionBox.findAll(
      By(SubscriptionBox.userModified, false),
      By(SubscriptionBox.animalType, AnimalType.Dog),
      By(SubscriptionBox.boxType, BoxType.healthAndWellness)
    )
  }

  def possiblePrice(subscriptionBox: SubscriptionBox, upgradedBox: Boolean = false): Double =
    if (subscriptionBox.subscriptionItems.toList.nonEmpty)
      subscriptionBox.pet.obj.map(basePrice(_, upgradedBox)).openOr(0d)
    else
      0d

  def basePrice(pet: Pet, upgradedBox: Boolean): Double = {
    val smallDogs = List(AnimalSize.DogSmallAdv, AnimalSize.DogSmallShld, AnimalSize.DogSmallZo)

    (pet.animalType.get, pet.size.get, upgradedBox) match {
      case (_, _, false)                                                  => 12.99
      case (AnimalType.Cat, _, _)                                         => 12.99
      case (AnimalType.Dog, dogSize, true) if smallDogs.contains(dogSize) => 24.99
      case (AnimalType.Dog, _, true)                                      => 27.99
    }
  }

  def createNewBox(
      subscription: Subscription,
      pet: Pet,
      upgradedBox: Boolean = false
  ): SubscriptionBox = {
    val fleaTick = FleaTick.find(By(FleaTick.size, pet.size.get))

    val newSubscriptionBox = SubscriptionBox.create
      .boxId(generateLongId)
      .subscription(subscription)
      .pet(pet)
      .animalType(pet.animalType.get)
      .fleaTick(fleaTick)
      .basePrice(basePrice(pet, upgradedBox))

    if (upgradedBox && pet.animalType == AnimalType.Dog)
      newSubscriptionBox.boxType(BoxType.healthAndWellness).saveMe()
    else
      newSubscriptionBox.boxType(BoxType.basic).saveMe()
  }
}

object BoxType extends Enumeration {
  val basic: BoxType.Value = Value("Flea & Tick Only")  // 0
  val healthAndWellness: BoxType.Value = Value("Health and Wellness") // 1
}
