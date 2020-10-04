package com.mypetdefense.jobs

import java.time.{LocalDate, ZoneId}
import java.util.Date

import com.mypetdefense.actor._
import com.mypetdefense.model._
import net.liftweb.mapper._
import org.quartz._

class SendForeverUpgradeEmailJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {

    val fourDaysBeforeStart: Date = Date.from(LocalDate.now.atStartOfDay(ZoneId.of("America/New_York")).minusDays(4).toInstant)
    val threeDaysBeforeStart: Date = Date.from(LocalDate.now.atStartOfDay(ZoneId.of("America/New_York")).minusDays(3).toInstant)

    val shipments = Shipment.findAll(
      By_>(Shipment.dateShipped, fourDaysBeforeStart),
      By_<(Shipment.dateShipped, threeDaysBeforeStart),
      By(Shipment.freeUpgradeSample, true)
    )
    
    for {
      shipment <- shipments
      subscription <- shipment.subscription.obj
        if !subscription.isUpgraded.get
      user <- subscription.user.obj
    } yield {
      EmailActor ! SendForeverUpgradeEmail(
        user,
        subscription
      )
    }
  }
}

object DailyForeverUpgradeEmailJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder.newJob(classOf[SendForeverUpgradeEmailJob])
    .withIdentity("DailyForeverUpgradeEmailJob")
    .build()

  val trigger: Trigger = TriggerBuilder.newTrigger()
    .withIdentity("DailyForeverUpgradeEmailJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 0 8 ? * * *"))
    .build()
}

object FrequentForeverUpgradeEmailJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder.newJob(classOf[SendTrackingEmailJob])
    .withIdentity("FrequentForeverUpgradeEmailJob")
    .build

  val trigger: Trigger = TriggerBuilder.newTrigger()
    .withIdentity("FrequentForeverUpgradeEmailJobTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 5 minutes
    .build
}
