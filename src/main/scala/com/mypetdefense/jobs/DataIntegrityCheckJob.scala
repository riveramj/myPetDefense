package com.mypetdefense.jobs

import com.mypetdefense.model.{Event, EventType, Pet, Shipment, Subscription, User}
import com.mypetdefense.util.DateHelper
import net.liftweb.common._
import org.quartz._

class DataIntegrityCheckJob extends ManagedJob {
  override def execute(context: JobExecutionContext): Unit = {
    val shipmentsPanicDate = DateHelper.threeDaysAgo

    val usersWithoutSub                   = User.allWithoutSubscription
    val subsWithoutUser                   = Subscription.allWithoutUser
    val petsWithoutBox                    = Pet.allWithoutBox
    val oldShipmentsWithoutTrackingNumber = Shipment.allWithoutTrackingNumber(shipmentsPanicDate)

    usersWithoutSub.foreach(logUserWithoutSub)
    subsWithoutUser.foreach(logSubsWithoutUser)
    petsWithoutBox.foreach(logPetsWithoutBox)
    oldShipmentsWithoutTrackingNumber.foreach(logOldShipmentWithoutTrackingNumber)
  }

  private def logOldShipmentWithoutTrackingNumber(shipment: Shipment) = {
    val title = "Pet doesn't have a box"
    val details =
      "During regular data integrity job, that pet was found, manual handling is needed."
    Event.createEvent(Empty, Empty, Full(shipment), Empty, EventType.Subscription, title, details)
  }

  private def logPetsWithoutBox(pet: Pet): Event = {
    val title = "Pet doesn't have a box"
    val details =
      "During regular data integrity job, that pet was found, manual handling is needed."
    Event.createEvent(Empty, Empty, Empty, Full(pet), EventType.Subscription, title, details)
  }

  private def logSubsWithoutUser(sub: Subscription): Event = {
    val title = "Subscription doesn't have an owner"
    val details =
      "During regular data integrity job, that subscription was found, manual handling is needed."
    Event.createEvent(Empty, Full(sub), Empty, Empty, EventType.Subscription, title, details)
  }

  private def logUserWithoutSub(user: User): Event = {
    val title = "User doesn't have a subscription"
    val details =
      "During regular data integrity job, that user was found, manual handling is needed."
    Event.createEvent(Full(user), Empty, Empty, Empty, EventType.User, title, details)
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
