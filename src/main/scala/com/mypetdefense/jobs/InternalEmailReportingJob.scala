package com.mypetdefense.jobs

import com.mypetdefense.actor._
import com.mypetdefense.model.{EmailReport, ReportType, Subscription}
import com.mypetdefense.service.ReportingService
import net.liftweb.mapper.By
import org.quartz._

import scala.collection.JavaConverters._

class InternalEmailReportingJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    val emailScope = context.getMergedJobDataMap.get("scope")

    val report = EmailReport.findAll(By(EmailReport.reportType, ReportType.DailyInternalReportEmail))
    val internalEmails = report.flatMap(_.emailRecords.toList).map(_.email.get)

    emailScope match {
      case "daily" =>
        val (
          newShipmentsYesterday: Int,
          paidShipmentsYesterday: Int,
          grossSalesYesterday: BigDecimal
        ) =
          ReportingService.yesterdayShipments

        val cancelsYesterday      = Subscription.yesterdayCancels
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
  val detail: JobDetail = JobBuilder
    .newJob(classOf[InternalEmailReportingJob])
    .withIdentity("WeeklySalesReportEmailJob")
    .usingJobData(new JobDataMap(Map("scope" -> "weekly").asJava))
    .build()

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("WeeklySalesReportEmailTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 0 8 ? * 2 *"))
    .build()
}

object DailyInternalReportEmailJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[InternalEmailReportingJob])
    .withIdentity("DailySalesReportEmailJob")
    .usingJobData(new JobDataMap(Map("scope" -> "daily").asJava))
    .build()

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("DailySalesReportEmailJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 0 7 ? * * *"))
    .build()
}

object FrequentInternalReportEmailJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[InternalEmailReportingJob])
    .withIdentity("FrequentSalesReportEmailJob")
    .usingJobData(new JobDataMap(Map("scope" -> "daily").asJava))
    .build

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("FrequentSalesReportEmailTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 5 minutes
    .build
}
