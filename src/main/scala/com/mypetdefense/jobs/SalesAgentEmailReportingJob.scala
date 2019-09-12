package com.mypetdefense.jobs

import org.quartz.{CronScheduleBuilder, TriggerBuilder, JobBuilder, JobExecutionContext}

import com.mypetdefense.service.ReportingService
import com.mypetdefense.actor._

class SalesAgentReportEmailJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {

    val dailyAgentData = ReportingService.findYesterdaySalesByAgent
    val monthlyAgentData = ReportingService.findMTDSalesByAgent

    val dailyAgencyData = ReportingService.findYesterdaySalesByAgency
    val monthlyAgencyData = ReportingService.findMTDSalesByAgency

    
    EmailActor ! DailySalesEmail(
      dailyAgentData,
      monthlyAgentData,
      dailyAgencyData,
      monthlyAgencyData,
      "mike.rivera@mypetdefense.com"
    )

      }
}

object DailyAgentSalesReportEmailJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[SalesAgentReportEmailJob])
    .withIdentity("DailyAgentSalesReportEmailJob")
    .build()

  val trigger = TriggerBuilder.newTrigger()
    .withIdentity("DailyAgentSalesReportEmailJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 03 6 ? * * *"))
    .build()
}

object FrequentAgentSalesReportEmailJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[SalesAgentReportEmailJob])
    .withIdentity("FrequentAgentSalesReportEmailJob")
    .build

  val trigger = TriggerBuilder.newTrigger()
    .withIdentity("FrequentAgentSalesReportEmailJobTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 5 minutes
    .build
}
