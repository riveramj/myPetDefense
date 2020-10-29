package com.mypetdefense.model

import net.liftweb._
import mapper._
import common._

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._

class Event extends LongKeyedMapper[Event] with IdPK with OneToMany[Long, Event] {
  def getSingleton: KeyedMetaMapper[Long, Event] = Event
  object eventId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object title          extends MappedString(this, 200)
  object details        extends MappedString(this, 1000)
  object notes          extends MappedString(this, 1000)
  object resolutionDate extends MappedDateTime(this)
  object eventDate      extends MappedDateTime(this)
  object eventType      extends MappedEnum(this, EventType)
  object eventStatus    extends MappedEnum(this, EventStatus)
  object subscription   extends MappedLongForeignKey(this, Subscription)
  object shipment       extends MappedLongForeignKey(this, Shipment)
  object user           extends MappedLongForeignKey(this, User)
  object pet            extends MappedLongForeignKey(this, Pet)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Event extends Event with LongKeyedMetaMapper[Event] {
  def createEvent(
      user: Box[User],
      subscription: Box[Subscription],
      shipment: Box[Shipment],
      pet: Box[Pet],
      eventType: EventType.Value,
      title: String,
      details: String,
      eventDate: Date = new Date()
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
  val Shipping, Billing, Signup, Pets, Product, User = Value
}

object EventStatus extends Enumeration {
  val Open, Pending, Resolved = Value
}
