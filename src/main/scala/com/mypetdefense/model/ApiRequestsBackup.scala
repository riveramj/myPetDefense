package com.mypetdefense.model

import java.util.Date

import net.liftweb._
import mapper._
import common._
import net.liftweb.json.JsonAST.JValue
import json._
import net.liftweb.common.Box.box2Option

class ApiRequestsBackup extends LongKeyedMapper[ApiRequestsBackup] with IdPK {
  override def getSingleton: KeyedMetaMapper[Long, ApiRequestsBackup] = ApiRequestsBackup

  object rawJson extends MappedString(this, 2000)

  object pets extends MappedString(this, 100)

  object agency extends MappedLongForeignKey(this, Agency)

  object user extends MappedLongForeignKey(this, User)

  object created extends MappedDateTime(this){
    override def defaultValue = new Date()
  }

  def createNewBackupRecord(referer: Box[Agency], rawJson: JValue): ApiRequestsBackup =
    ApiRequestsBackup.create
      .agency(referer)
      .rawJson(prettyRender(rawJson))
      .saveMe

  def updateUser(record: ApiRequestsBackup, user: User): ApiRequestsBackup = record.user(user).saveMe()

  def updatePets(record: ApiRequestsBackup, pets: List[Box[Pet]]): ApiRequestsBackup =
    record.pets(pets.map(_.fold("")(_.id.get.toString)).mkString(",")).saveMe()

}

object ApiRequestsBackup extends ApiRequestsBackup with LongKeyedMetaMapper[ApiRequestsBackup]
