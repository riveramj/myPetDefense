package com.mypetdefense.jobs

import com.mypetdefense.model._
import com.mypetdefense.util.DateHelper.DateOps
import org.quartz._

class SubscriptionBoxProductsUpdateJob extends ManagedJob {

  override def execute(context: JobExecutionContext): Unit = tryToUpdateBoxes()

  def tryToUpdateBoxes(): Unit = {
    val nextRegularSchedule    = ProductSchedule.getNextRegularSchedule
    val actualRegularSchedule  = ProductSchedule.getActiveRegularSchedule
    val nextFirstBoxSchedule   = ProductSchedule.getNextScheduleForFirstBox
    val actualFirstBoxSchedule = ProductSchedule.getActiveScheduleForFirstBox

    executeNextOrActiveRegularSchedule(nextRegularSchedule, actualRegularSchedule)
    executeFirstBoxSchedule(nextFirstBoxSchedule, actualFirstBoxSchedule)
  }

  def executeNextOrActiveRegularSchedule(
      nextRegularSchedule: Option[ProductSchedule],
      actualRegularSchedule: Option[ProductSchedule]
  ): Unit =
    nextRegularSchedule
      .map(executeRegularSchedule(_, onlyForYesterdayUsers = false))
      .flatMap { nextSchedule =>
        nextSchedule.makeActive
        actualRegularSchedule.map(_.complete)
      }
      .orElse(actualRegularSchedule.map(executeRegularSchedule(_, onlyForYesterdayUsers = true)))

  def executeFirstBoxSchedule(
      nextFirstBoxSchedule: Option[ProductSchedule],
      actualFirstBoxSchedule: Option[ProductSchedule]
  ): Unit =
    nextFirstBoxSchedule.foreach { nextSchedule =>
      nextSchedule.makeActive
      actualFirstBoxSchedule.foreach(_.complete)
    }

  private def executeRegularSchedule(
      schedule: ProductSchedule,
      onlyForYesterdayUsers: Boolean
  ): ProductSchedule = {
    val scheduleItems   = schedule.scheduledItems.toList
    val newProducts     = scheduleItems.flatMap(_.product.toList)
    val unmodifiedBoxes = SubscriptionBox.getAllUnmodifiedByUser
    val boxesToUpdate =
      if (onlyForYesterdayUsers)
        unmodifiedBoxes.filter(_.subscription.obj.exists(_.createdAt.get.isYesterday))
      else
        unmodifiedBoxes
    boxesToUpdate.foreach { box =>
      box.subscriptionItems.toList.foreach(_.delete_!)
      newProducts.map(SubscriptionItem.createSubscriptionItem(_, box))
    }
    schedule
  }

}

object DailySubscriptionBoxProductsUpdateJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[SubscriptionBoxProductsUpdateJob])
    .withIdentity("DailySubscriptionBoxProductsUpdateJob")
    .build()

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("DailySubscriptionBoxProductsUpdateJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 5 0 ? * * *"))
    .build()
}
