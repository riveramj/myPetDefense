package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class Retailor extends LongKeyedMapper[Retailor] with IdPK {
  def getSingleton = Retailor
  object retailorId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object name extends MappedString(this, 100)
  object agent extends MappedLongForeignKey(this, User)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewRetailor(
    name: String, 
    agent: User
  ) = {
    Retailor.create
    .retailorId(generateLongId)
    .name(name)
    .agent(agent)
    .saveMe
  }
}

object Retailor extends Retailor with LongKeyedMetaMapper[Retailor]


