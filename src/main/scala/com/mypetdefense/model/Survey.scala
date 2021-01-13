package com.mypetdefense.model

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
  object couponApplied extends MappedZonedDateTime(this)
  object sentDate      extends MappedZonedDateTime(this, useNowAsDefault = true)
  object respondedDate extends MappedZonedDateTime(this)
  object createdAt     extends MappedZonedDateTime(this, useNowAsDefault = true)
}

object Survey extends Survey with LongKeyedMetaMapper[Survey] {
  def createNewSurvey(user: User): Survey = {
    Survey.create
      .surveyId(generateLongId)
      .user(user)
      .saveMe
  }
}
