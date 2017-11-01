package com.mypetdefense.model 

import net.liftweb._
  import mapper._
  import common._
  import util._
    import Helpers._

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._

class Groupon extends LongKeyedMapper[Groupon] with IdPK with OneToMany[Long, Groupon] {
  def getSingleton = Groupon
  object grouponId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object grouponCode extends MappedString(this, 100)
  object freeMonths extends MappedInt(this)
  object user extends MappedLongForeignKey(this, User)
  object redeemedAt extends MappedDateTime(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Groupon extends Groupon with LongKeyedMetaMapper[Groupon] {
  def createNewGroupon(grouponCode: String, freeMonths: Int, user: Box[User] = Empty) = {
    Groupon.create
      .grouponId(generateLongId)
      .grouponCode(grouponCode.toLowerCase)
      .freeMonths(freeMonths)
      .user(user)
      .saveMe
  }
}
