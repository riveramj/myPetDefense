package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date 

class Agency extends LongKeyedMapper[Agency] with IdPK with OneToMany[Long, Agency] {
  def getSingleton = Agency
  object agencyId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object name extends MappedString(this, 100)
  object customers extends MappedOneToMany(User, User.referer)
  object members extends MappedOneToMany(User, User.agency)
  object coupons extends MappedOneToMany(Coupon, Coupon.agency)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewAgency(
    name: String
  ) = {
    Agency.create
    .agencyId(generateLongId)
    .name(name)
    .saveMe
  }
}

object Agency extends Agency with LongKeyedMetaMapper[Agency]
