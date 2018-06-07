package com.mypetdefense.jobs

import org.quartz.{CronScheduleBuilder, TriggerBuilder, JobBuilder, JobExecutionContext}

import com.mypetdefense.service.ReportingService
import com.mypetdefense.actor._

class DailyAgentReportEmailJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {

    val agentData = ReportingService.findYesterdaySalesByAgent("TPP")
    
    EmailActor ! DailySalesEmail(agentData, "mike.rivera@mypetdefense.com")
    EmailActor ! DailySalesEmail(agentData, "silvia@thirdpartypet.com")
    EmailActor ! DailySalesEmail(agentData, "mike@canineregistrations.com")
    EmailActor ! DailySalesEmail(agentData, "melissa@thirdpartypet.com")
  }
}

object WeeklySalesReportEmailJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[DailyAgentReportEmailJob])
    .withIdentity("WeeklySalesReportEmailJob")
    .build()

  val trigger = TriggerBuilder.newTrigger()
    .withIdentity("WeeklySalesReportEmailTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 0 8 ? * 2 *"))
    .build()
}

object DailySalesReportEmailJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[DailyAgentReportEmailJob])
    .withIdentity("DailySalesReportEmailJob")
    .build()

  val trigger = TriggerBuilder.newTrigger()
    .withIdentity("DailySalesReportEmailJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 0 7 ? * * *"))
    .build()
}

object FrequentSalesReportEmailJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[DailyAgentReportEmailJob])
    .withIdentity("FrequentSalesReportEmailJob")
    .build

  val trigger = TriggerBuilder.newTrigger()
    .withIdentity("FrequentSalesReportEmailTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */5 * ? * *")) // fire every 5 minutes
    .build
}
