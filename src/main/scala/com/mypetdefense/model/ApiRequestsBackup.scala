package com.mypetdefense.model

import java.util.Date

import com.mypetdefense.snippet.NewPet
import net.liftweb._
import mapper._
import common._
import net.liftweb.json.JsonAST.JValue
import json._
import com.mypetdefense.snippet.shop.UseNewCard.typeHints

class ApiRequestsBackup extends LongKeyedMapper[ApiRequestsBackup] with IdPK {
  override def getSingleton: KeyedMetaMapper[Long, ApiRequestsBackup] = ApiRequestsBackup

  object rawJson extends MappedText(this)

  object pets extends MappedText(this)

  object agency extends MappedLongForeignKey(this, Agency)

  object user extends MappedLongForeignKey(this, User)

  object created extends MappedDateTime(this){
    override def defaultValue = new Date()
  }

  def createNewBackupRecord(referer: Box[Agency], pets: List[NewPet], rawJson: JValue): ApiRequestsBackup =
    ApiRequestsBackup.create
      .agency(referer)
      .rawJson(prettyRender(rawJson))
      .pets(pets.mkString(","))
      .saveMe

  def updateUser(record: ApiRequestsBackup, user: User): ApiRequestsBackup = record.user(user).saveMe()

}

object ApiRequestsBackup extends ApiRequestsBackup with LongKeyedMetaMapper[ApiRequestsBackup]
