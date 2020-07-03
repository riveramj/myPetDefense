package com.mypetdefense.model

import net.liftweb._
  import mapper._

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._

class Subscription extends LongKeyedMapper[Subscription] with IdPK with OneToMany[Long, Subscription] {
  def getSingleton = Subscription
  object subscriptionId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object user extends MappedLongForeignKey(this, User)
  object stripeSubscriptionId extends MappedString(this, 100)
  object startDate extends MappedDateTime(this)
  object renewalDate extends MappedDateTime(this)
  object nextShipDate extends MappedDateTime(this)
  object priceCode extends MappedString(this, 100)
  object contractLength extends MappedInt(this) {
    override def defaultValue = 0
  }
  object shipments extends MappedOneToMany(Shipment, Shipment.subscription)
  object status extends MappedEnum(this, Status) {
    override def defaultValue = Status.Active
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
  object cancellationDate extends MappedDateTime(this)
  object cancellationReason extends MappedString(this, 100)
  object cancellationComment extends MappedText(this)
  object subscriptionBoxes extends MappedOneToMany(SubscriptionBox, SubscriptionBox.subscription)
  object tags extends MappedOneToMany(TaggedItem, TaggedItem.subscription)

  def refresh = Subscription.find(By(Subscription.subscriptionId, subscriptionId.get))

  def getPets = user.obj.map(_.activePets).openOr(Nil)

  def cancel = {
    this
      .status(Status.Cancelled)
      .cancellationDate(new Date())
      .saveMe
  }

  def getPetAndProducts = this.subscriptionBoxes.map { box =>
    (box.pet.obj, box.fleaTick.obj)
  }

  def createNewSubscription(
    user: User,
    stripeSubscriptionId: String,
    startDate: Date,
    nextShipDate: Date,
    priceCode: String = Price.defaultPriceCode,
    contractLength: Int = 0
  ) = {
    Subscription.create
    .subscriptionId(generateLongId)
    .user(user)
    .stripeSubscriptionId(stripeSubscriptionId)
    .startDate(startDate)
    .nextShipDate(nextShipDate)
    .priceCode(priceCode)
    .contractLength(contractLength)
    .saveMe
  }

  def getMonthlyCost = {
    this.subscriptionBoxes.map { box =>
      box.basePrice.get + box.addOnProducts.map(_.price.get).sum
    }.sum
  }
}

object Subscription extends Subscription with LongKeyedMetaMapper[Subscription]

object Status extends Enumeration {
  val Active, Inactive, UserSuspended, BillingSuspended, Cancelled, Paused = Value
}
