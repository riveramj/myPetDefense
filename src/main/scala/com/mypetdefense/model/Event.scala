package com.mypetdefense.model

import java.time.ZonedDateTime

import com.mypetdefense.AppConstants.DefaultTimezone
import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common._
import net.liftweb.mapper._

class Event extends LongKeyedMapper[Event] with IdPK with OneToMany[Long, Event] {
  def getSingleton: KeyedMetaMapper[Long, Event] = Event

  object eventId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object title          extends MappedString(this, 20000)
  object details        extends MappedString(this, 20000)
  object notes          extends MappedString(this, 20000)
  object resolutionDate extends MappedZonedDateTime(this)
  object eventDate      extends MappedZonedDateTime(this)
  object eventType      extends MappedEnum(this, EventType)
  object eventStatus    extends MappedEnum(this, EventStatus)
  object subscription   extends MappedLongForeignKey(this, Subscription)
  object shipment       extends MappedLongForeignKey(this, Shipment)
  object user           extends MappedLongForeignKey(this, User)
  object pet            extends MappedLongForeignKey(this, Pet)
  object createdAt      extends MappedZonedDateTime(this, useNowAsDefault = true)
}

object Event extends Event with LongKeyedMetaMapper[Event] {
  def createEvent(
      user: Box[User] = Empty,
      subscription: Box[Subscription] = Empty,
      shipment: Box[Shipment] = Empty,
      pet: Box[Pet] = Empty,
      eventType: EventType.Value,
      title: String,
      details: String,
      eventDate: ZonedDateTime = ZonedDateTime.now(DefaultTimezone)
  ): Event = {
    Event.create
      .eventId(generateLongId)
      .user(user)
      .subscription(subscription)
      .shipment(shipment)
      .pet(pet)
      .eventType(eventType)
      .eventDate(eventDate)
      .title(title)
      .details(details)
      .eventStatus(EventStatus.Open)
      .saveMe
  }

  def unresolvedEvents: List[Event] = findAll(NotBy(Event.eventStatus, EventStatus.Resolved))
}

object EventType extends Enumeration {
  val Shipping, Billing, Signup, Subscription, Pets, Product, User = Value
}

object EventStatus extends Enumeration {
  val Open, Pending, Resolved = Value
}
