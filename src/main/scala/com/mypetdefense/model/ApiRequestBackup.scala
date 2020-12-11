package com.mypetdefense.model

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator.generateLongId
import net.liftweb.common._
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json._
import net.liftweb.mapper._

class ApiRequestBackup extends LongKeyedMapper[ApiRequestBackup] with IdPK {
  override def getSingleton: KeyedMetaMapper[Long, ApiRequestBackup] = ApiRequestBackup

  object apiRequestId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object rawJson extends MappedString(this, 2000)

  object agency extends MappedLongForeignKey(this, Agency)

  object user extends MappedLongForeignKey(this, User)

  object created extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

}

object ApiRequestBackup extends ApiRequestBackup with LongKeyedMetaMapper[ApiRequestBackup] {
  def createNewBackupRecord(referer: Box[Agency], rawJson: JValue): ApiRequestBackup =
    ApiRequestBackup.create
      .apiRequestId(generateLongId)
      .agency(referer)
      .rawJson(prettyRender(rawJson))
      .saveMe

  def updateUser(record: ApiRequestBackup, user: User): ApiRequestBackup =
    record.user(user).saveMe()
}
