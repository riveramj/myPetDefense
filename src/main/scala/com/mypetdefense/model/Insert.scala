package com.mypetdefense.model

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common._
import net.liftweb.mapper._

class Insert extends LongKeyedMapper[Insert] with IdPK {
  def getSingleton: KeyedMetaMapper[Long, Insert] = Insert
  object insertId extends MappedLong(this){
    override def dbIndexed_? = true
    override def defaultValue: Long = generateLongId
  }
  object name extends MappedString(this, 100)
  object itemNumber extends MappedString(this, 100)
  object weight extends MappedDouble(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewInsert(name: String, itemNumber: String, weight: Double): Insert = {
    Insert.create
      .name(name)
      .itemNumber(itemNumber)
      .weight(weight)
      .saveMe
  }

  def welcomeInsert: Box[Insert] = Insert.find(By(Insert.name,"Welcome Insert"))

  def tryUpgrade: Box[Insert] = Insert.find(By(Insert.name, "Free Upgraded Box Trial Insert"))

  def productBrochure: Box[Insert] = Insert.find(By(Insert.name, "Summer Product Brochure"))

  def tppWelcomeInsert: Box[Insert] = Insert.find(By(Insert.name, "TPP Registrations Welcome Insert"))

  def petlandWelcomeInsert: Box[Insert] = Insert.find(By(Insert.name, "Petland Welcome Insert"))
}

object Insert extends Insert with LongKeyedMetaMapper[Insert]
