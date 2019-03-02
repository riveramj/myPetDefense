package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class Insert extends LongKeyedMapper[Insert] with IdPK {
  def getSingleton = Insert
  object insertId extends MappedLong(this){
    override def dbIndexed_? = true
    override def defaultValue = generateLongId
  }
  object name extends MappedString(this, 100)
  object itemNumber extends MappedString(this, 100)
  object weight extends MappedDouble(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewInsert(name: String, itemNumber: String, weight: Double) = {
    Insert.create
      .name(name)
      .itemNumber(itemNumber)
      .weight(weight)
      .saveMe
  }

  def welcomeInsert = Insert.find(By(Insert.name,"Welcome Insert"))

  def tppWelcomeInsert = Insert.find(By(Insert.name, "TPP Registrations Welcome Insert"))

  def petlandWelcomeInsert = Insert.find(By(Insert.name, "Petland Welcome Insert"))
}

object Insert extends Insert with LongKeyedMetaMapper[Insert]
