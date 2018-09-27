package com.mypetdefense.jobs

import org.quartz.{CronScheduleBuilder, TriggerBuilder, JobBuilder, JobExecutionContext}

import com.mypetdefense.service.ReportingService
import com.mypetdefense.actor._

class SalesAgentReportEmailJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {

    val dailAgentData = ReportingService.findYesterdaySalesByAgent("TPP")
    val monthlyAgentData = ReportingService.findMTDSalesByAgent("TPP")
    
    EmailActor ! DailySalesEmail(dailAgentData, monthlyAgentData, "mike.rivera@mypetdefense.com")
    EmailActor ! DailySalesEmail(dailAgentData, monthlyAgentData, "silvia@thirdpartypet.com")
    EmailActor ! DailySalesEmail(dailAgentData, monthlyAgentData, "mike@canineregistrations.com")
    EmailActor ! DailySalesEmail(dailAgentData, monthlyAgentData, "melissa@thirdpartypet.com")
    EmailActor ! DailySalesEmail(dailAgentData, monthlyAgentData, "toni@thirdpartypet.com")
    EmailActor ! DailySalesEmail(dailAgentData, monthlyAgentData, "katie@thirdpartypet.com")
    EmailActor ! DailySalesEmail(dailAgentData, monthlyAgentData, "alicia@thirdpartypet.com")
  }
}

object DailyAgentSalesReportEmailJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[SalesAgentReportEmailJob])
    .withIdentity("DailyAgentSalesReportEmailJob")
    .build()

  val trigger = TriggerBuilder.newTrigger()
    .withIdentity("DailyAgentSalesReportEmailJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 0 7 ? * * *"))
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
