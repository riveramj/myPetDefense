package com.mypetdefense.jobs

import com.mypetdefense.model.{StatisticsSnapshot, Status, User}
import net.liftweb.mapper.By
import org.quartz._

class RecordStatisticsSnapshotJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    val users = User.findAll(By(User.status, Status.Active))
    val usersByAgency = users.groupBy(_.referer.obj)
    val boxesByTypeAndAgency = usersByAgency.map { case (agency, users) =>
      val activeBoxes = users.flatMap(_.subscription.obj.toList
        .filter(_.status == Status.Active)
        .flatMap(_.subscriptionBoxes.toList.filter(_.status.get == Status.Active))
      )
      (agency, activeBoxes.groupBy(_.boxType.get))
    }

    for {
      (possibleAgency, boxesByType) <- boxesByTypeAndAgency
      agency <- possibleAgency.toList
      (boxType, boxes) <- boxesByType
    } yield {
      StatisticsSnapshot.createDailySnapShot(boxes.size, boxType, agency)
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
