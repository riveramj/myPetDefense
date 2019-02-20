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
  object parent extends MappedLongForeignKey(this, Agency)
  object agencyType extends MappedEnum(this, AgencyType)
  object customers extends MappedOneToMany(User, User.referer)
  object members extends MappedOneToMany(User, User.agency)
  object coupons extends MappedOneToMany(Coupon, Coupon.agency)
  object petlandStore extends MappedBoolean(this) {
    override def defaultValue = false
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewAgency(
    name: String,
    agencyType: AgencyType.Value = AgencyType.Headquarters,
    parent: Box[Agency] = Empty
  ) = {
    Agency.create
    .agencyId(generateLongId)
    .name(name)
    .agencyType(agencyType)
    .parent(parent)
    .saveMe
  }

  def getHQFor(agency: Agency): Agency = {
    {
      for {
        possibleHQ <- agency.parent.obj
      } yield {
        if (possibleHQ.parent.isDefined)
          getHQFor(possibleHQ)
        else
          possibleHQ
      }
    } openOr {
      agency
    }
  }

  def getAllHQ = Agency.findAll(By(Agency.agencyType, AgencyType.Headquarters))

  def getAllChildrenCustomers(agency: Agency): List[User] = {
    
    val childrenAgencies = Agency.findAll(By(Agency.parent, agency))

    if (childrenAgencies.isEmpty) {
      agency.customers.toList
    } else{
      childrenAgencies.map { child =>
        getAllChildrenCustomers(child)
      }.flatten
    }
  }
}

object Agency extends Agency with LongKeyedMetaMapper[Agency]

object AgencyType extends Enumeration {
  val Headquarters, Store = Value
}
