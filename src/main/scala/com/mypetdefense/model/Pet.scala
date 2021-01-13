package com.mypetdefense.model

import java.time.ZonedDateTime
import java.util.Date

import com.mypetdefense.util.DateHelper._
import com.mypetdefense.util.RandomIdGenerator._
import com.mypetdefense.util.TitleCase
import net.liftweb.common._
import net.liftweb.mapper._

class Pet extends LongKeyedMapper[Pet] with IdPK {
  def getSingleton: KeyedMetaMapper[Long, Pet] = Pet

  object petId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object user            extends MappedLongForeignKey(this, User)
  object box             extends MappedLongForeignKey(this, SubscriptionBox)
  object name            extends MappedString(this, 100)
  object breed           extends MappedString(this, 100)
  object animalType      extends MappedEnum(this, AnimalType)
  object size            extends MappedEnum(this, AnimalSize)
  object adultSize       extends MappedEnum(this, AnimalSize)
  object birthday        extends MappedZonedDateTime(this) // TODO: maybe should be date only
  object nextGrowthDelay extends MappedInt(this)
  object sentDogTag extends MappedBoolean(this) {
    override def defaultValue = false
  }
  object status extends MappedEnum(this, Status) {
    override def defaultValue: Status.Value = Status.Active
  }
  object createdAt extends MappedZonedDateTime(this, useNowAsDefault = true)
}

object Pet extends Pet with LongKeyedMetaMapper[Pet] {
  def createNewPet(
      user: User,
      name: String,
      product: FleaTick,
      breed: String,
      whelpDate: Box[ZonedDateTime],
      dummy: Boolean // TODO: remove after deleting deprecated version
  ): Pet =
    createNewPet(
      user,
      name,
      product.animalType.get,
      product.size.get,
      whelpDate,
      breed,
      dummy
    )

  @deprecated("Migrate to ZonedDateTime-based version", since = "0.1-SNAPSHOT")
  def createNewPet(
      user: User,
      name: String,
      product: FleaTick,
      breed: String,
      whelpDate: Box[Date]
  ): Pet =
    createNewPet(user, name, product, breed, whelpDate.map(_.toZonedDateTime), dummy = false)

  // TODO: copy default arguments after deleting deprecated version
  def createNewPet(
      user: User,
      name: String,
      animalType: AnimalType.Value,
      size: AnimalSize.Value,
      whelpDate: Box[ZonedDateTime],
      breed: String,
      dummy: Boolean // TODO: remove after deleting deprecated version
  ): Pet =
    Pet.create
      .petId(generateLongId)
      .user(user)
      .name(TitleCase(name))
      .animalType(animalType)
      .size(size)
      .breed(breed)
      .birthday(whelpDate.openOr(null))
      .saveMe

  @deprecated("Migrate to ZonedDateTime-based version", since = "0.1-SNAPSHOT")
  def createNewPet(
      user: User,
      name: String,
      animalType: AnimalType.Value,
      size: AnimalSize.Value,
      whelpDate: Box[Date] = Empty,
      breed: String = ""
  ): Pet =
    createNewPet(
      user,
      name,
      animalType,
      size,
      whelpDate.map(_.toZonedDateTime),
      breed,
      dummy = false
    )

  def createNewPet(pet: Pet, user: User): Pet =
    pet.user(user).saveMe

  def notCancelledWithoutBox: List[Pet] =
    Pet.findAll(NotBy(Pet.status, Status.Cancelled), NullRef(Pet.box))
}

object AnimalType extends Enumeration {
  val Dog, Cat = Value
}

object AnimalSize extends Enumeration {
  val CatSmall: AnimalSize.Value  = Value("1.5-5") // 0
  val CatMedium: AnimalSize.Value = Value("5-9")   // 1
  val CatLarge: AnimalSize.Value  = Value("9+")    // 2

  val DogSmallAdv: AnimalSize.Value  = Value("3-10")  // 3
  val DogMediumAdv: AnimalSize.Value = Value("11-20") // 4
  val DogLargeAdv: AnimalSize.Value  = Value("21-55") // 5
  val DogXLargeAdv: AnimalSize.Value = Value("55+")   // 6

  val DogSmallZo: AnimalSize.Value  = Value("5-22")   // 7
  val DogMediumZo: AnimalSize.Value = Value("23-44")  // 8
  val DogLargeZo: AnimalSize.Value  = Value("45-88")  // 9
  val DogXLargeZo: AnimalSize.Value = Value("89-132") // 10

  val DogSmallShld: AnimalSize.Value  = Value("5-15")  // 11
  val DogMediumShld: AnimalSize.Value = Value("16-33") // 12
  val DogLargeShld: AnimalSize.Value  = Value("34-66") // 13
  val DogXLargeShld: AnimalSize.Value = Value("66+")   // 14

  val CatAllSize: AnimalSize.Value = Value("1.5+") // 15
}
