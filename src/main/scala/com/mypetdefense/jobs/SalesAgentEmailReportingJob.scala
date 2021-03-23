package com.mypetdefense.jobs

import com.mypetdefense.actor._
import com.mypetdefense.model.{EmailReport, ReportType}
import com.mypetdefense.service.ReportingService
import net.liftweb.mapper.By
import org.quartz._

class SalesAgentReportEmailJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {

    val dailyAgentData   = ReportingService.findYesterdaySalesByAgent
    val monthlyAgentData = ReportingService.findMTDSalesByAgent

    val dailyAgencyData   = ReportingService.findYesterdaySalesByAgency
    val monthlyAgencyData = ReportingService.findMTDSalesByAgency

    val report = EmailReport.findAll(By(EmailReport.reportType, ReportType.DailyTPPAgentSalesReportEmail))
    val emails = report.flatMap(_.emailRecords.toList).map(_.email.get)

    emails.map { email =>
      EmailActor ! DailySalesEmail(
        dailyAgentData,
        monthlyAgentData,
        dailyAgencyData,
        monthlyAgencyData,
        email
      )
    }
  }
}

object DailyAgentSalesReportEmailJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[SalesAgentReportEmailJob])
    .withIdentity("DailyAgentSalesReportEmailJob")
    .build()

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("DailyAgentSalesReportEmailJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 0 7 ? * * *"))
    .build()
}

object FrequentAgentSalesReportEmailJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[SalesAgentReportEmailJob])
    .withIdentity("FrequentAgentSalesReportEmailJob")
    .build

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("FrequentAgentSalesReportEmailJobTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 5 minutes
    .build
}
