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
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewInsert(name: String, itemNumber: String) = {
    Insert.create
      .name(name)
      .itemNumber(itemNumber)
      .saveMe
  }

  def welcomeInsert = Insert.find(By(Insert.name,"Welcome Brochure"))

  def tppWelcomeInsert = Insert.find(By(Insert.name, "TPP Welcome Insert"))
}

object Insert extends Insert with LongKeyedMetaMapper[Insert]
