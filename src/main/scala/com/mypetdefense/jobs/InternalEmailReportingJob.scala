package com.mypetdefense.jobs

import org.quartz._
import collection.JavaConversions._

import com.mypetdefense.service.ReportingService
import com.mypetdefense.actor._

class InternalEmailReportingJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    val emailScope = context.getMergedJobDataMap.get("scope")

    val internalEmails = List("mike.rivera@mypetdefense.com", "liz.shinn@mypetdefense.com", "calvin.leach@mypetdefense.com")

    emailScope match {
      case "daily" => 
        val (newShipmentsYesterday: Int, paidShipmentsYesterday: Int, grossSalesYesterday: Double) = ReportingService.yesterdayShipments

        val cancelsYesterday = ReportingService.yesterdayCancels
        val cancelsYesterdayCount = cancelsYesterday.size

        internalEmails.map { email =>
          EmailActor ! InternalDailyEmail(
            newShipmentsYesterday,
            paidShipmentsYesterday,
            grossSalesYesterday,
            cancelsYesterdayCount,
            email
          )
        }
      case _ =>
    }
  }
}

object WeeklyInteralReportEmailJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[InternalEmailReportingJob])
    .withIdentity("WeeklySalesReportEmailJob")
    .usingJobData(new JobDataMap(Map("scope" -> "weekly")))
    .build()

    val trigger = TriggerBuilder.newTrigger()
      .withIdentity("WeeklySalesReportEmailTrigger")
      .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 0 8 ? * 2 *"))
    .build()
}

object DailyInternalReportEmailJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[InternalEmailReportingJob])
    .withIdentity("DailySalesReportEmailJob")
    .usingJobData(new JobDataMap(Map("scope" -> "daily")))
    .build()

  val trigger = TriggerBuilder.newTrigger()
    .withIdentity("DailySalesReportEmailJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 0 7 ? * * *"))
    .build()
}

object FrequentInternalReportEmailJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[InternalEmailReportingJob])
    .withIdentity("FrequentSalesReportEmailJob")
    .usingJobData(new JobDataMap(Map("scope" -> "daily")))
    .build

  val trigger = TriggerBuilder.newTrigger()
    .withIdentity("FrequentSalesReportEmailTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 5 minutes
    .build
}

