package com.mypetdefense.jobs

import com.mypetdefense.model._
import net.liftweb.common.Box
import net.liftweb.mapper.By
import org.quartz._

class RecordStatisticsSnapshotJob extends ManagedJob {
  case class SnapshotStatistics(agency: Box[Agency], subscriptionCount: Int, boxCountByType: Map[BoxType.Value, Int])

  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    val users = User.findAll(By(User.status, Status.Active))
    val usersByAgency = users.groupBy(_.referer.obj)
    val boxesByTypeAndAgency = usersByAgency.map { case (agency, users) =>
      val subscriptions = users.flatMap(_.subscription.obj.toList
        .filter(_.status == Status.Active)
      )
      val activeBoxes = subscriptions.flatMap(_.subscriptionBoxes.toList.filter(_.status.get == Status.Active))
      val boxCountByType = activeBoxes.groupBy(_.boxType.get).map { case (boxType, subscriptions) =>
        (boxType, subscriptions.size)
      }

      SnapshotStatistics(agency, subscriptions.size, boxCountByType)
    }

    for {
      snapShotStatistic <- boxesByTypeAndAgency.toList
      agency <- snapShotStatistic.agency.toList
      (boxType, boxCount) <- snapShotStatistic.boxCountByType
    } yield {
      StatisticsSnapshot.createDailySnapShot(snapShotStatistic.subscriptionCount, boxCount, boxType, agency)
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
