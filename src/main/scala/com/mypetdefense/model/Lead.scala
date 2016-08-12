package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class Lead extends LongKeyedMapper[Lead] with IdPK {
  def getSingleton = Lead
  object leadId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object firstName extends MappedString(this, 100)
  object lastName extends MappedString(this, 100)
  object email extends MappedEmail(this, 50)
  object phone extends MappedString(this, 100)
  object referer extends MappedLongForeignKey(this, Agent)
  object parent extends MappedLongForeignKey(this, User)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def name = s"${firstName} ${lastName}"

  def createNewLead(
    firstName: String,
    lastName: String,
    email: String,
    phone: String,
    referer: Agent
  ) = {
    Lead.create
    .leadId(generateLongId)
    .firstName(firstName)
    .lastName(lastName)
    .email(email)
    .phone(phone)
    .referer(referer)
    .saveMe
  }
}

object Lead extends Lead with LongKeyedMetaMapper[Lead]

