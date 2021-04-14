package com.mypetdefense.jobs

import com.mypetdefense.model._
import com.mypetdefense.service.ReportingService
import org.quartz._

class RecordStatisticsSnapshotJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    val subsByAgency = ReportingService.getActiveSubscriptionsByAgency
    val snapshotStatistics = ReportingService.getSnapshotStatisticsForSubsByAgency(subsByAgency)

    for {
      snapShotStatistic <- snapshotStatistics
      agency = snapShotStatistic.agency
      boxType = snapShotStatistic.boxStatistics.boxType
      subscriptionCount = snapShotStatistic.boxStatistics.subscriptionCount
      boxCount = snapShotStatistic.boxStatistics.boxCount
    } yield {
      StatisticsSnapshot.createDailySnapShot(subscriptionCount, boxCount, boxType, agency)
    }
  }
}

object DailyRecordStatisticsSnapshotJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[RecordStatisticsSnapshotJob])
    .withIdentity("DailyRecordStatisticsSnapshotJob")
    .build()

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("DailyRecordStatisticsSnapshotJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 56 23 ? * * *"))
    .build()
}

object FrequentRecordStatisticsSnapshotJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[RecordStatisticsSnapshotJob])
    .withIdentity("FrequentRecordStatisticsSnapshotJob")
    .build

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("FrequentRecordStatisticsSnapshotJobTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 5 minutes
    .build
}
