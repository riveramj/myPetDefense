package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class Pet extends LongKeyedMapper[Pet] with IdPK {
  def getSingleton = Pet
  object petId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object user extends MappedLongForeignKey(this, User)
  object name extends MappedString(this, 100)
  object breed extends MappedString(this, 100)
  object animalType extends MappedEnum(this, AnimalType)
  object size extends MappedEnum(this, AnimalSize)
  object birthday extends MappedDateTime(this)
  object product extends MappedLongForeignKey(this, Product)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewPet(
    user: User,
    name: String,
    animalType: AnimalType.Value,
    size: AnimalSize.Value,
    product: Product
  ) = {
    Pet.create
    .petId(generateLongId)
    .user(user)
    .name(name)
    .animalType(animalType)
    .size(size)
    .product(product)
    .saveMe
  }
}

object Pet extends Pet with LongKeyedMetaMapper[Pet]

object AnimalType extends Enumeration {
  val Dog, Cat = Value
}

object AnimalSize extends Enumeration {
  val CatSmall = Value("1.5-5")
  val CatMedium = Value("5-9")
  val CatLarge = Value("9+")
  val Small, Medium, Large, XLarge = Value
}
