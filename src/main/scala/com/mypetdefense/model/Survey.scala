package com.mypetdefense.model

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.mapper._

class Survey extends LongKeyedMapper[Survey] with IdPK {
  def getSingleton: KeyedMetaMapper[Long, Survey] = Survey
  object surveyId extends MappedLong(this) {
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
}

object Survey extends Survey with LongKeyedMetaMapper[Survey] {
  def createNewSurvey(user: User): Survey = {
    Survey.create
      .surveyId(generateLongId)
      .user(user)
      .saveMe
  }
}
