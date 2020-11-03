package com.mypetdefense.jobs

import com.mypetdefense.model._
import com.mypetdefense.util.DateHelper
import net.liftweb.common._
import net.liftweb.mapper._
import org.quartz._

class DataIntegrityCheckJob extends ManagedJob {
  override def execute(context: JobExecutionContext): Unit = checkDataIntegrity()

  private[jobs] def checkDataIntegrity(): Unit = {
    val shipmentsWithoutTrackingPanicDate  = DateHelper.threeDaysAgo
    val shipmentsWithoutLineItemsPanicDate = DateHelper.sixtyDaysAgo

    val usersWithoutSub = User.notCancelledWithoutSubscription
    val subsWithoutUser = Subscription.notCancelledWithoutUser
    val petsWithoutBox  = Pet.notCancelledWithoutBox
    val oldShipmentsWithoutTrackingNumber =
      Shipment.notCancelledWithoutTrackingNumber(shipmentsWithoutTrackingPanicDate)
    val oldsShipmentsZeroLineItems =
      Shipment.notCancelledWithEmptyLineItems(shipmentsWithoutLineItemsPanicDate)

    usersWithoutSub.foreach(logUserWithoutSub)
    subsWithoutUser.foreach(logSubsWithoutUser)
    petsWithoutBox.foreach(logPetsWithoutBox)
    oldShipmentsWithoutTrackingNumber.foreach(logOldShipmentWithoutTrackingNumber)
    oldsShipmentsZeroLineItems.foreach(logOldShipmentsWithoutLineItems)
  }

  private def findExistingUnresolvedShipmentEvent(shipment: Shipment): Box[Event] = Event.find(
    By(Event.shipment, shipment),
    By(Event.eventType, EventType.Shipping),
    NotBy(Event.eventStatus, EventStatus.Resolved)
  )

  private def findExistingUnresolvedPetsEvent(pet: Pet): Box[Event] = Event.find(
    By(Event.pet, pet),
    By(Event.eventType, EventType.Pets),
    NotBy(Event.eventStatus, EventStatus.Resolved)
  )

  private def findExistingUnresolvedSubscriptionWithoutUserEvent(sub: Subscription): Box[Event] =
    Event.find(
      By(Event.subscription, sub),
      By(Event.eventType, EventType.Subscription),
      NullRef(Event.user),
      NotBy(Event.eventStatus, EventStatus.Resolved)
    )

  private def findExistingUserWithoutSubEvent(user: User): Box[Event] =
    Event.find(
      By(Event.user, user),
      By(Event.eventType, EventType.User),
      NullRef(Event.subscription),
      NotBy(Event.eventStatus, EventStatus.Resolved)
    )

  private def logOldShipmentsWithoutLineItems(shipment: Shipment): Event = {
    val maybeExistingEvent = findExistingUnresolvedShipmentEvent(shipment)
    maybeExistingEvent.getOrElse(createOldShipmentWithoutLineItemsEvent(shipment))
  }

  private def logOldShipmentWithoutTrackingNumber(shipment: Shipment): Event = {
    val maybeExistingEvent = findExistingUnresolvedShipmentEvent(shipment)
    maybeExistingEvent.getOrElse(createShipmentWithoutTrackingNumberEvent(shipment))
  }

  private def logPetsWithoutBox(pet: Pet): Event = {
    val maybeExistingEvent = findExistingUnresolvedPetsEvent(pet)
    maybeExistingEvent.getOrElse(createPetWithoutBoxEvent(pet))
  }

  private def logSubsWithoutUser(sub: Subscription): Event = {
    val maybeExistingEvent = findExistingUnresolvedSubscriptionWithoutUserEvent(sub)
    maybeExistingEvent.getOrElse(createSubWithoutUserEvent(sub))
  }

  private def logUserWithoutSub(user: User): Event = {
    val maybeExistingEvent = findExistingUserWithoutSubEvent(user)
    maybeExistingEvent.getOrElse(createUserWithoutSubEvent(user))
  }

  private def createOldShipmentWithoutLineItemsEvent(shipment: Shipment): Event = {
    val title        = "Shipment doesn't have shipping line items."
    val subscription = shipment.subscription
    val shipmentUser = subscription.flatMap(_.user)
    val details =
      "During regular data integrity job, that shipment was found, manual handling is needed."
    Event.createEvent(
      user = shipmentUser,
      subscription = subscription,
      shipment = Full(shipment),
      eventType = EventType.Shipping,
      title = title,
      details = details
    )
  }

  private def createShipmentWithoutTrackingNumberEvent(shipment: Shipment): Event = {
    val title        = "Shipment doesn't have a tracking number for three days."
    val subscription = shipment.subscription
    val shipmentUser = subscription.flatMap(_.user)
    val details =
      "During regular data integrity job, that shipment was found, manual handling is needed."
    Event.createEvent(
      user = shipmentUser,
      subscription = subscription,
      shipment = Full(shipment),
      eventType = EventType.Shipping,
      title = title,
      details = details
    )
  }

  private def createPetWithoutBoxEvent(pet: Pet) = {
    val title   = "Pet doesn't have a box"
    val petUser = pet.user
    val details =
      "During regular data integrity job, that pet was found, manual handling is needed."
    Event.createEvent(
      user = petUser,
      subscription = petUser.flatMap(_.subscription),
      pet = Full(pet),
      eventType = EventType.Pets,
      title = title,
      details = details
    )
  }

  private def createSubWithoutUserEvent(sub: Subscription): Event = {
    val title = "Subscription doesn't have an owner"
    val details =
      "During regular data integrity job, that subscription was found, manual handling is needed."
    Event.createEvent(
      subscription = Full(sub),
      eventType = EventType.Subscription,
      title = title,
      details = details
    )
  }

  private def createUserWithoutSubEvent(user: User): Event = {
    val title = "User doesn't have a subscription"
    val details =
      "During regular data integrity job, that user was found, manual handling is needed."
    Event.createEvent(
      user = Full(user),
      subscription = user.subscription,
      eventType = EventType.User,
      title = title,
      details = details
    )
  }
}

object OneTimePerDayDataIntegrityCheckJob extends TriggeredJob {
  override def detail: JobDetail =
    JobBuilder
      .newJob(classOf[DataIntegrityCheckJob])
      .withIdentity("OneTimePerDayDataIntegrityCheckJob")
      .build()

  override def trigger: Trigger =
    TriggerBuilder
      .newTrigger()
      .withIdentity("OneTimePerDayDataIntegrityCheckJob")
      .startNow()
      .withSchedule(CronScheduleBuilder.cronSchedule("0 0 3 ? * * *")) // At 03:00:00am every day
      .build()
}

object FrequentDataIntegrityCheckJob extends TriggeredJob {
  override def detail: JobDetail =
    JobBuilder
      .newJob(classOf[DataIntegrityCheckJob])
      .withIdentity("FrequentDataIntegrityCheckJob")
      .build()

  override def trigger: Trigger =
    TriggerBuilder
      .newTrigger()
      .withIdentity("FrequentDataIntegrityCheckJob")
      .startNow()
      .withSchedule(
        CronScheduleBuilder.cronSchedule("0 0/5 * ? * * *")
      ) // At second :00, every 5 minutes starting at minute :00, of every hour
      .build()
}
