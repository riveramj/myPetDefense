package com.mypetdefense.jobs

import net.liftweb._
  import common._
  import mapper._
  import util.Helpers._ 

import com.mypetdefense.service.ParentService
import com.mypetdefense.actor._
import com.mypetdefense.model._

import org.quartz.{CronScheduleBuilder, TriggerBuilder, JobBuilder, JobExecutionContext}

import java.util.Date
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import java.time.{LocalDate, ZoneId, LocalDateTime, Period}
import java.time.format.DateTimeFormatter

class GrowthNotifyJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    def currentDate = LocalDateTime.now()

    val allActiveSubscriptions = Subscription.findAll(
      By(Subscription.status, Status.Active),
      By(Subscription.priceCode, Price.currentTppPriceCode)
    )

    val upcomingSubscription = allActiveSubscriptions.filter { subscription =>
      val nextShipDate = tryo(subscription.nextShipDate.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate)

      val nextShipDateDayOfYear = nextShipDate.map(_.getDayOfYear).openOr(0)

      (nextShipDateDayOfYear - currentDate.getDayOfYear == 7)
    }

    val readyToGrowPetsUsers: List[(Pet, String, User)] = upcomingSubscription.flatMap { subscription =>
      ParentService.findGrowingPets(subscription)
    }


    readyToGrowPetsUsers.map { case (pet, newFleaTick, user) =>
      EmailActor ! NotifyParentGrowthRate(pet, newFleaTick, user)
    }
  }
}

object OneWeekNotifyGrowthJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[GrowthNotifyJob])
    .withIdentity("OneWeeksNotifyGrowthJob")
    .build()

  val trigger = TriggerBuilder.newTrigger()
    .withIdentity("OneWeeksNotifyGrowthJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 0 7 ? * * *"))
    .build()
}

object FrequentNotifyGrowthJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[GrowthNotifyJob])
    .withIdentity("FrequentNotifyGrowthJob")
    .build

  val trigger = TriggerBuilder.newTrigger()
    .withIdentity("FrequentNotifyGrowthJobTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 5 minutes
    .build
}
