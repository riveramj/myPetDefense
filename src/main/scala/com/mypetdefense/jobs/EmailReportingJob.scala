package com.mypetdefense.jobs

import org.quartz.{CronScheduleBuilder, TriggerBuilder, JobBuilder, JobExecutionContext}

class SalesReportEmailJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    println("job!")

    //emailActor ! SendRoomSummaryNotificationEmail(notificationInterval(context), frequency)
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
