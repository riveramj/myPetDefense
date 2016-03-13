package com.fleaTick.model

import net.liftweb.mapper._
import java.util.Date

class Pet extends LongKeyedMapper[Pet] with IdPK with OneToMany[Long, Pet] {
  def getSingleton = Pet
  object PetId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object user extends MappedLongForeignKey(this, User)
  object name extends MappedString(this, 100)
  object breed extends MappedString(this, 100)
  object animalType extends MappedEnum(this, AnimalType)
  object size extends MappedEnum(this, AnimalSize)
  object birthday extends MappedDateTime(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Pet extends Pet with LongKeyedMetaMapper[Pet]

object AnimalType extends Enumeration {
  val Dog, Cat = Value
}

object AnimalSize extends Enumeration {
  val Small, Medium, Large, XLarge = Value
}
