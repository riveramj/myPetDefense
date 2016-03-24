package com.fleaTick.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.fleaTick.util.RandomIdGenerator._

import java.util.Date

class Pet extends LongKeyedMapper[Pet] with IdPK {
  def getSingleton = Pet
  object petId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object parent extends MappedLongForeignKey(this, Parent)
  object name extends MappedString(this, 100)
  object breed extends MappedString(this, 100)
  object animalType extends MappedEnum(this, AnimalType)
  object size extends MappedEnum(this, AnimalSize)
  object birthday extends MappedDateTime(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewPet(
    parent: Parent,
    name: String,
    animalType: AnimalType.Value,
    size: AnimalSize.Value
  ) = {
    Pet.create
    .petId(generateLongId)
    .parent(parent)
    .name(name)
    .animalType(animalType)
    .size(size)
    .saveMe
  }
}

object Pet extends Pet with LongKeyedMetaMapper[Pet]

object AnimalType extends Enumeration {
  val Dog, Cat = Value
}

object AnimalSize extends Enumeration {
  val Small, Medium, Large, XLarge = Value
}
