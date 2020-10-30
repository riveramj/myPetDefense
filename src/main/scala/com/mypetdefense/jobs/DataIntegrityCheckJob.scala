package com.mypetdefense.jobs

import com.mypetdefense.model.{Event, EventType, Pet, Shipment, Subscription, User}
import com.mypetdefense.util.DateHelper
import net.liftweb.common._
import org.quartz._

class DataIntegrityCheckJob extends ManagedJob {
  override def execute(context: JobExecutionContext): Unit = checkDataIntegrity()

  private[jobs] def checkDataIntegrity(): Unit = {
    val shipmentsPanicDate = DateHelper.threeDaysAgo

    val usersWithoutSub = User.notCancelledWithoutSubscription
    val subsWithoutUser = Subscription.notCancelledWithoutUser
    val petsWithoutBox  = Pet.notCancelledWithoutBox
    val oldShipmentsWithoutTrackingNumber =
      Shipment.cancelledWithoutTrackingNumber(shipmentsPanicDate)

    usersWithoutSub.foreach(logUserWithoutSub)
    subsWithoutUser.foreach(logSubsWithoutUser)
    petsWithoutBox.foreach(logPetsWithoutBox)
    oldShipmentsWithoutTrackingNumber.foreach(logOldShipmentWithoutTrackingNumber)
  }

  private def logOldShipmentWithoutTrackingNumber(shipment: Shipment): Event = {
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

  private def logPetsWithoutBox(pet: Pet): Event = {
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

  private def logSubsWithoutUser(sub: Subscription): Event = {
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

  private def logUserWithoutSub(user: User): Event = {
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
