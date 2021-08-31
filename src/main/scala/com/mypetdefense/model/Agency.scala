package com.mypetdefense.model

import com.mypetdefense.util.DateHelper.currentDate
import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common._
import net.liftweb.mapper._

import java.util.Date
import scala.annotation.tailrec

class Agency extends LongKeyedMapper[Agency] with IdPK with OneToMany[Long, Agency] {
  def getSingleton: KeyedMetaMapper[Long, Agency] = Agency
  object agencyId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object name       extends MappedString(this, 100)
  object parent     extends MappedLongForeignKey(this, Agency)
  object stores     extends MappedOneToMany(Agency, Agency.parent)
  object agencyType extends MappedEnum(this, AgencyType)
  object customers  extends MappedOneToMany(User, User.referer)
  object members    extends MappedOneToMany(User, User.agency)
  object storeCode  extends MappedString(this, 100)
  object coupons    extends MappedOneToMany(Coupon, Coupon.agency)
  object petlandStore extends MappedBoolean(this) {
    override def defaultValue = false
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def currentMonthShipments: List[Shipment] =
    this.findAllShipments.filter { shipment =>
      val mailedDate = shipment.getMailedDateOfShipment
      mailedDate.map(_.getMonth == currentDate.getMonth).openOr(false) &&
      mailedDate.map(_.getYear == currentDate.getYear).openOr(false)
    }

  def findAllShipments: List[Shipment] =
    for {
      customer     <- this.customers.toList
      subscription <- customer.subscription.toList
      shipment     <- subscription.shipments.toList.sortBy(_.dateProcessed.get.getTime)
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
    @tailrec
    def getAllCustomers(possibleAgencies: List[Agency], customers: List[User]): List[User] ={
      possibleAgencies match {
        case Nil => customers
        case head :: tail =>
          getAllCustomers(tail ++ head.stores.toList, customers ++ head.customers.toList)
      }
    }

    getAllCustomers(agency.stores.toList, agency.customers.toList)
  }

  def getUsersForAgency(agencyName: String): List[User] = {
    val agency = Agency.find(By(Agency.name, agencyName))
    agency.map(_.customers.toList).openOr(Nil)
  }

  //TODO clean up this
  def getTotalUsers(agencyName: String): List[User] =
    //Agency.find(By(Agency.name, agencyName)).map(getAllChildrenCustomers).openOr(Nil)
    Agency.findAll(By(Agency.agencyType, AgencyType.Headquarters)).flatMap(getAllChildrenCustomers)
}

object AgencyType extends Enumeration {
  val Headquarters, Store = Value
}
