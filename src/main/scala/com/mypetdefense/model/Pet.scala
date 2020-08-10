package com.mypetdefense.model

import net.liftweb._
import mapper._
import common._
import util._
import com.mypetdefense.util.RandomIdGenerator._
import com.mypetdefense.util.TitleCase
import java.util.Date

class Pet extends LongKeyedMapper[Pet] with IdPK {
  def getSingleton: KeyedMetaMapper[Long, Pet] = Pet
  object petId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object user extends MappedLongForeignKey(this, User)
  object box extends MappedLongForeignKey(this, SubscriptionBox)
  object name extends MappedString(this, 100)
  object breed extends MappedString(this, 100)
  object animalType extends MappedEnum(this, AnimalType)
  object size extends MappedEnum(this, AnimalSize)
  object adultSize extends MappedEnum(this, AnimalSize)
  object birthday extends MappedDateTime(this)
  object nextGrowthDelay extends MappedInt(this)
  object sentDogTag extends MappedBoolean(this) {
    override def defaultValue = false
  }
  object status extends MappedEnum(this, Status) {
    override def defaultValue: Status.Value = Status.Active
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def refresh: Box[Pet] = Pet.find(By(Pet.petId, petId.get))

  def createNewPet(
    user: User,
    name: String,
    animalType: AnimalType.Value,
    size: AnimalSize.Value,
    product: FleaTick,
    whelpDate: Box[Date] = Empty,
    breed: String = ""
  ): Pet = {
    Pet.create
    .petId(generateLongId)
    .user(user)
    .name(TitleCase(name))
    .animalType(animalType)
    .size(size)
    .breed(breed)
    .birthday(whelpDate.openOr(null))
    .saveMe
  }

  def createNewPet(
    user: User,
    name: String,
    product: FleaTick,
    breed: String,
    whelpDate: Box[Date]
  ): Pet = {
    createNewPet(
      user,
      name,
      product.animalType.get,
      product.size.get,
      product,
      whelpDate,
      breed
    )
  }

  def createNewPet(pet: Pet, user: User): Pet = {
    pet.user(user).saveMe
  }
}

object Pet extends Pet with LongKeyedMetaMapper[Pet]

object AnimalType extends Enumeration {
  val Dog, Cat = Value
}

object AnimalSize extends Enumeration {
  val CatSmall: AnimalSize.Value = Value("1.5-5")
  val CatMedium: AnimalSize.Value = Value("5-9")
  val CatLarge: AnimalSize.Value = Value("9+")
  
  val DogSmallAdv: AnimalSize.Value = Value("3-10")
  val DogMediumAdv: AnimalSize.Value = Value("11-20")
  val DogLargeAdv: AnimalSize.Value = Value("21-55")
  val DogXLargeAdv: AnimalSize.Value = Value("55+")

  val DogSmallZo: AnimalSize.Value = Value("5-22")
  val DogMediumZo: AnimalSize.Value = Value("23-44")
  val DogLargeZo: AnimalSize.Value = Value("45-88")
  val DogXLargeZo: AnimalSize.Value = Value("89-132")

  val DogSmallShld: AnimalSize.Value = Value("5-15")
  val DogMediumShld: AnimalSize.Value = Value("16-33")
  val DogLargeShld: AnimalSize.Value = Value("34-66")
  val DogXLargeShld: AnimalSize.Value = Value("66+")
  
  val CatAllSize: AnimalSize.Value = Value("1.5+")
}
