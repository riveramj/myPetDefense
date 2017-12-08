package com.mypetdefense.model 

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class Survey extends LongKeyedMapper[Survey] with IdPK {
  def getSingleton = Survey
  object surveyId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object user extends MappedLongForeignKey(this, User)
  object ratingGiven extends MappedBoolean(this) {
    override def defaultValue = false
  }
  object testimonialGiven extends MappedBoolean(this) {
    override def defaultValue = false
  }
  object couponApplied extends MappedDateTime(this)
  object sentDate extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
  object respondedDate extends MappedDateTime(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewSurvey(user: User) = {
    Survey.create
    .surveyId(generateLongId)
    .user(user)
    .saveMe
  }
}

object Survey extends Survey with LongKeyedMetaMapper[Survey]
