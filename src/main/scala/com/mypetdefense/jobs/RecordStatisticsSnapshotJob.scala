package com.mypetdefense.jobs

import com.mypetdefense.model._
import net.liftweb.mapper.By
import org.quartz._

class RecordStatisticsSnapshotJob extends ManagedJob {
  case class SnapshotStatistics(agency: Agency, boxStatistics: BoxStatistics)
  case class BoxStatistics(boxType: BoxType.Value, boxCount: Int, subscriptionCount: Int)

  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    val activeSubscriptions = Subscription.findAll(By(Subscription.status, Status.Active))
    val agencyAndSubscriptions =
      for {
        sub <- activeSubscriptions
        user <- sub.user.obj
        agency <- user.referer.obj
      } yield (agency, sub)

    val subsByAgency = agencyAndSubscriptions.groupBy(_._1).collect { case (agency, agencySubs) =>
      agency -> agencySubs.map(_._2)
    }

    val snapshotStatistics =
      (for {
        (agency, subscriptions) <- subsByAgency
        subscription <- subscriptions
        activeBoxes = subscription.subscriptionBoxes.filter(_.status.get == Status.Active)
      } yield {
        val boxStatistics: Iterable[BoxStatistics] = activeBoxes.groupBy(_.boxType.get).map { case (boxType, boxes) =>
          val subIds = boxes.map(_.subscription.get).distinct
          BoxStatistics(boxType, boxes.size, subIds.size)
        }

        boxStatistics.map(SnapshotStatistics(agency, _))
      }).flatten

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
    .withSchedule(CronScheduleBuilder.cronSchedule("0 26 14 ? * * *"))
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
