package com.mypetdefense.model

import com.mypetdefense.service.ReportingService.petlandName
import com.mypetdefense.util.DateHelper.currentDate
import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common._
import net.liftweb.mapper._

class Agency extends LongKeyedMapper[Agency] with IdPK with OneToMany[Long, Agency] {
  def getSingleton: KeyedMetaMapper[Long, Agency] = Agency
  object agencyId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object name       extends MappedString(this, 100)
  object parent     extends MappedLongForeignKey(this, Agency)
  object agencyType extends MappedEnum(this, AgencyType)
  object customers  extends MappedOneToMany(User, User.referer)
  object members    extends MappedOneToMany(User, User.agency)
  object storeCode  extends MappedString(this, 100)
  object coupons    extends MappedOneToMany(Coupon, Coupon.agency)
  object petlandStore extends MappedBoolean(this) {
    override def defaultValue = false
  }
  object createdAt extends MappedZonedDateTime(this, useNowAsDefault = true)

  def currentMonthShipments: List[Shipment] = this.findAllShipments.filter { shipment =>
    val mailedDate = shipment.getMailedDateOfShipment
    mailedDate.map(_.getMonth == currentDate.getMonth).openOr(false) &&
    mailedDate.map(_.getYear == currentDate.getYear).openOr(false)
  }

  def findAllShipments: List[Shipment] =
    for {
      customer     <- this.customers.toList
      subscription <- customer.subscription.toList
      shipment     <- subscription.shipments.toList.sortBy(_.dateProcessed.get.toInstant.toEpochMilli)
    } yield {
      shipment
    }
}

object Agency extends Agency with LongKeyedMetaMapper[Agency] {
  lazy val mpdAgency: Box[Agency] = Agency.find(By(Agency.name, "My Pet Defense"))
  lazy val tppAgency: Box[Agency] = Agency.find(By(Agency.name, "TPP"))

  def createNewAgency(
      name: String,
      agencyType: AgencyType.Value = AgencyType.Headquarters,
      parent: Box[Agency] = Empty,
      storeCode: String = "",
      petlandStore: Boolean = false
  ): Agency = {
    Agency.create
      .agencyId(generateLongId)
      .name(name)
      .agencyType(agencyType)
      .parent(parent)
      .storeCode(storeCode)
      .petlandStore(petlandStore)
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

  def getAllHQ: List[Agency] = Agency.findAll(By(Agency.agencyType, AgencyType.Headquarters))

  def getAllChildrenCustomers(agency: Agency): List[User] = {

    val childrenAgencies = Agency.findAll(By(Agency.parent, agency))

    if (childrenAgencies.isEmpty) {
      agency.customers.toList
    } else {
      childrenAgencies.flatMap { child => getAllChildrenCustomers(child) }
    }
  }

  def getUsersForAgency(agencyName: String): List[User] = {
    val agency = Agency.find(By(Agency.name, agencyName))
    agency.map(_.customers.toList).openOr(Nil)
  }

  //TODO clean up this
  def getTotalUsers(agencyName: String): List[User] =
    if (agencyName != "TPP")
      Agency.find(By(Agency.name, agencyName)).map(_.customers.toList).getOrElse(Nil)
    else {
      val puppySpot =
        Agency.find(By(Agency.name, "PuppySpot")).map(_.customers.toList).getOrElse(Nil)
      val petland =
        Agency.find(By(Agency.name, petlandName)).map(Agency.getAllChildrenCustomers).getOrElse(Nil)

      puppySpot ++ petland
    }
}

object AgencyType extends Enumeration {
  val Headquarters, Store = Value
}
