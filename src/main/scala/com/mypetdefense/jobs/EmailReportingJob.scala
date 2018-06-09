package com.mypetdefense.jobs

import org.quartz.{CronScheduleBuilder, TriggerBuilder, JobBuilder, JobExecutionContext}

import com.mypetdefense.service.ReportingService
import com.mypetdefense.actor._

class DailyAgentReportEmailJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {

    val dailAgentData = ReportingService.findYesterdaySalesByAgent("TPP")
    val monthlyAgentData = ReportingService.findMTDSalesByAgent("TPP")
    
    EmailActor ! DailySalesEmail(dailAgentData, monthlyAgentData, "rivera.mj@gmail.com")
    //EmailActor ! DailySalesEmail(dailAgentData, monthlyAgentData, "silvia@thirdpartypet.com")
    //EmailActor ! DailySalesEmail(dailAgentData, monthlyAgentData, "mike@canineregistrations.com")
    //EmailActor ! DailySalesEmail(dailAgentData, monthlyAgentData, "melissa@thirdpartypet.com")
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
