package com.mypetdefense.model

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator.generateLongId
import net.liftweb._
import net.liftweb.json.JsonAST.JValue
import mapper._
import common._
import json._

class ApiRequestsBackup extends LongKeyedMapper[ApiRequestsBackup] with IdPK {
  override def getSingleton: KeyedMetaMapper[Long, ApiRequestsBackup] = ApiRequestsBackup

  object apiRequestsId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object rawJson extends MappedString(this, 2000)

  object agency extends MappedLongForeignKey(this, Agency)

  object user extends MappedLongForeignKey(this, User)

  object created extends MappedDateTime(this){
    override def defaultValue = new Date()
  }

  def createNewBackupRecord(referer: Box[Agency], rawJson: JValue): ApiRequestsBackup =
    ApiRequestsBackup.create
      .apiRequestsId(generateLongId)
      .agency(referer)
      .rawJson(prettyRender(rawJson))
      .saveMe

  def updateUser(record: ApiRequestsBackup, user: User): ApiRequestsBackup = record.user(user).saveMe()

}

object ApiRequestsBackup extends ApiRequestsBackup with LongKeyedMetaMapper[ApiRequestsBackup]
