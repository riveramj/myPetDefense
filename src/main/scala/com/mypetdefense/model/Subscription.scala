package com.mypetdefense.model

import java.time.{LocalDate, ZoneId}

import net.liftweb._
import mapper._
import java.util.Date

import com.mypetdefense.util.DateHelper._
import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common.Box
import net.liftweb.util.Helpers.tryo

import scala.collection.mutable

class Subscription
    extends LongKeyedMapper[Subscription]
    with IdPK
    with OneToMany[Long, Subscription] {
  def getSingleton: KeyedMetaMapper[Long, Subscription] = Subscription
  object subscriptionId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object user                  extends MappedLongForeignKey(this, User)
  object promptedUpgrade       extends MappedBoolean(this)
  object isUpgraded            extends MappedBoolean(this)
  object freeUpgradeSampleDate extends MappedDateTime(this)
  object stripeSubscriptionId  extends MappedString(this, 100)
  object startDate             extends MappedDateTime(this)
  object renewalDate           extends MappedDateTime(this)
  object nextShipDate          extends MappedDateTime(this)
  object priceCode             extends MappedString(this, 100)
  object contractLength extends MappedInt(this) {
    override def defaultValue = 0
  }
  object shipments extends MappedOneToMany(Shipment, Shipment.subscription)
  object status extends MappedEnum(this, Status) {
    override def defaultValue: Status.Value = Status.Active
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
  object cancellationDate    extends MappedDateTime(this)
  object cancellationReason  extends MappedString(this, 100)
  object cancellationComment extends MappedText(this)
  object subscriptionBoxes   extends MappedOneToMany(SubscriptionBox, SubscriptionBox.subscription)
  object tags                extends MappedOneToMany(TaggedItem, TaggedItem.subscription)

  def refresh: Box[Subscription] =
    Subscription.find(By(Subscription.subscriptionId, subscriptionId.get))

  def getPets: Seq[Pet] = user.obj.map(_.activePets).openOr(Nil)

  def cancel: Subscription = {
    this
      .status(Status.Cancelled)
      .cancellationDate(new Date())
      .saveMe
  }

  def getPetAndProducts: mutable.Buffer[(Box[Pet], Box[FleaTick])] = this.subscriptionBoxes.map {
    box => (box.pet.obj, box.fleaTick.obj)
  }

  def createNewSubscription(
      user: Box[User],
      stripeSubscriptionId: String,
      startDate: Date,
      nextShipDate: Date,
      priceCode: String = Price.defaultPriceCode,
      isUpgraded: Boolean = false,
      contractLength: Int = 0
  ): Subscription = {
    Subscription.create
      .subscriptionId(generateLongId)
      .user(user)
      .stripeSubscriptionId(stripeSubscriptionId)
      .startDate(startDate)
      .nextShipDate(nextShipDate)
      .priceCode(priceCode)
      .isUpgraded(isUpgraded)
      .contractLength(contractLength)
      .saveMe
  }

  def getMonthlyCost: Double = {
    this.subscriptionBoxes.map { box => box.basePrice.get + box.addOnProducts.map(_.price.get).sum }.sum
  }

  def filterMailedShipments: List[Shipment] = {
    this.shipments.toList.filter { shipment =>
      val dateProcessed = shipment.getProcessDateOfShipment

      val legacyShipment_? = dateProcessed.isBefore(LocalDate.parse("2018-01-01"))

      !shipment.getMailedDateOfShipment.isEmpty || legacyShipment_?
    }
  }

  def getStartDateOfSubscription: String =
    tryo(signupCancelDateFormat.format(this.startDate.get)).openOr("")

  def getCancelDateOfSubscription: String =
    tryo(signupCancelDateFormat.format(this.cancellationDate.get)).openOr("")

  def getCreatedDateOfSubscription: LocalDate =
    this.createdAt.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate

  def getCancelledDateOfSubscription: Box[LocalDate] =
    tryo(this.cancellationDate.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate)

  def getNextShipDate: LocalDate =
    this.nextShipDate.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate

  def findNewYTDSubscriptionsLastYear: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, yearDayOneLastYear),
      By_<(Subscription.createdAt, currentDayLastYearEnd)
    )
  }

  def findCancelledMtdSubscriptions: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.cancellationDate, monthDayOne)
    )
  }

  def findNewYTDSubscriptionsLastMonth: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, yearDayOne),
      By_<(Subscription.createdAt, todayLastMonthEnd)
    )
  }

  def findNewYTDSubscriptions: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, yearDayOne)
    )
  }

  def findNewMTDSubscriptionsLastYear: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, monthDayOneLastYear),
      By_<(Subscription.createdAt, currentDayLastYearEnd)
    )
  }

  def findNewMTDSubscriptionsLastMonth: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, monthDayOneLastMonth),
      By_<(Subscription.createdAt, currentDayLastMonthEnd)
    )
  }

  def findNewMTDSubscriptions: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, monthDayOne)
    )
  }

  def findNewTodaySubscriptionsLastYear: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, todayLastYear),
      By_<(Subscription.createdAt, todayLastYearEnd)
    )
  }

  def findNewTodaySubscriptionsLastMonth: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, todayLastMonth),
      By_<(Subscription.createdAt, todayLastMonthEnd)
    )
  }

  def findNewTodaySubscriptions: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, nowDate)
    )
  }

  def findCurrentMonthUpcomingSubscriptions: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.nextShipDate, tomorrowStart),
      By_<(Subscription.nextShipDate, beginngNextMonth)
    )
  }

  def yesterdayCancels: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.cancellationDate, yesterdayStart),
      By_<(Subscription.cancellationDate, yesterdayEnd)
    )
  }

}

object Subscription extends Subscription with LongKeyedMetaMapper[Subscription]

object Status extends Enumeration {
  val Active, Inactive, UserSuspended, BillingSuspended, Cancelled, Paused = Value
}
