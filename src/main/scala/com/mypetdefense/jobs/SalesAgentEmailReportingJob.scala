package com.mypetdefense.jobs

import com.mypetdefense.actor._
import com.mypetdefense.service.ReportingService
import org.quartz._

class SalesAgentReportEmailJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {

    val dailyAgentData   = ReportingService.findYesterdaySalesByAgent
    val monthlyAgentData = ReportingService.findMTDSalesByAgent

    val dailyAgencyData   = ReportingService.findYesterdaySalesByAgency
    val monthlyAgencyData = ReportingService.findMTDSalesByAgency

    val emails = EmailReport.findAll(By(EmailReport.reportType, ReportType.DailyTPPAgentSalesReportEmail))
      
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
