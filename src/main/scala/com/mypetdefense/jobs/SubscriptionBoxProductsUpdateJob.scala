package com.mypetdefense.jobs

import com.mypetdefense.model._
import org.quartz._

class SubscriptionBoxProductsUpdateJob extends ManagedJob {

  override def execute(context: JobExecutionContext): Unit = tryToUpdateBoxes()

  def tryToUpdateBoxes(): Unit = {
    val schedule = ProductSchedule.getNextSchedule
    schedule.foreach(updateSubscriptionBoxes)
  }

  def updateSubscriptionBoxes(schedule: ProductSchedule): Unit = {
    val scheduleItems = schedule.scheduledItems.toList
    val newProducts   = scheduleItems.flatMap(_.product.toList)
    val boxes         = SubscriptionBox.getAllUnmodifiedByUser
    boxes.foreach { box =>
      box.subscriptionItems.toList.foreach(_.delete_!)
      newProducts.map(SubscriptionItem.createSubscriptionItem(_, box))
    }
    schedule.scheduleStatus(ProductScheduleStatus.Completed).saveMe()
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
