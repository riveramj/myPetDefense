package com.mypetdefense.jobs

import org.quartz.{CronScheduleBuilder, TriggerBuilder, JobBuilder, JobExecutionContext}

import com.mypetdefense.service.ReportingService
import com.mypetdefense.actor._

class SalesReportEmailJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {

    val agentData = ReportingService.findYesterdaySalesByAgent("TPP")
    
    EmailActor ! DailySalesEmail(agentData, "mike.rivera@mypetdefense.com")
  }
}

object WeeklySalesReportEmailJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[SalesReportEmailJob])
    .withIdentity("WeeklySalesReportEmailJob")
    .build()

  val trigger = TriggerBuilder.newTrigger()
    .withIdentity("WeeklySalesReportEmailTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 0 8 ? * 2 *"))
    .build()
}

object FrequentSalesReportEmailJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[SalesReportEmailJob])
    .withIdentity("FrequentSalesReportEmailJob")
    .build

  val trigger = TriggerBuilder.newTrigger()
    .withIdentity("FrequentSalesReportEmailTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 1 minute
    .build
}
