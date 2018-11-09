package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class PetBox extends LongKeyedMapper[PetBox] with IdPK with OneToMany[Long, PetBox] {
  def getSingleton = PetBox
  object petBoxId extends MappedLong(this) {
    override def dbIndexed_? = true
    override def defaultValue = generateLongId
  }
  object name extends MappedString(this, 100)
  object price extends MappedDouble(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewPetBox(name: String, price: Double) = {
    PetBox.create
    .name(name)
    .price(price)
    .saveMe
  }
}

object PetBox extends PetBox with LongKeyedMetaMapper[PetBox]
