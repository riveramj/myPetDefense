package com.mypetdefense.jobs

import com.mypetdefense.model._
import com.mypetdefense.util.DateHelper.DateOps
import net.liftweb.common.Box
import org.quartz._

class SubscriptionBoxProductsUpdateJob extends ManagedJob {

  override def execute(context: JobExecutionContext): Unit = executeOp(context) {
    tryToUpdateBoxes()
  }

  def tryToUpdateBoxes(): Unit = {
    val nextRegularSchedule    = ProductSchedule.getNextRegularSchedule
    val actualRegularSchedule  = ProductSchedule.getActiveRegularSchedule
    val nextFirstBoxSchedule   = ProductSchedule.getNextScheduleForFirstBox
    val actualFirstBoxSchedule = ProductSchedule.getActiveScheduleForFirstBox

    executeNextOrActiveRegularSchedule(nextRegularSchedule, actualRegularSchedule)
    executeFirstBoxSchedule(nextFirstBoxSchedule, actualFirstBoxSchedule)
  }

  def executeNextOrActiveRegularSchedule(
      nextRegularSchedule: Box[ProductSchedule],
      actualRegularSchedule: Box[ProductSchedule]
  ): Unit =
    nextRegularSchedule
      .map(executeRegularSchedule(_, onlyForYesterdayUsers = false))
      .flatMap { nextSchedule =>
        nextSchedule.makeActive
        actualRegularSchedule.map(_.complete)
      }
      .orElse(actualRegularSchedule.map(executeRegularSchedule(_, onlyForYesterdayUsers = true)))

  def executeFirstBoxSchedule(
      nextFirstBoxSchedule: Box[ProductSchedule],
      actualFirstBoxSchedule: Box[ProductSchedule]
  ): Unit =
    nextFirstBoxSchedule.foreach { nextSchedule =>
      nextSchedule.makeActive
      actualFirstBoxSchedule.foreach(_.complete)
    }

  private def executeRegularSchedule(
      schedule: ProductSchedule,
      onlyForYesterdayUsers: Boolean
  ): ProductSchedule = {
    val scheduleItems     = schedule.scheduledItems.toList
    val newProducts       = scheduleItems.flatMap(_.product.toList)
    val unmodifiedBoxes   = SubscriptionBox.getAllUnmodifiedDogHealthWellness
    val dentalPowder      = Product.dentalPowder
    val smallSizes        = List(AnimalSize.DogSmallAdv, AnimalSize.DogSmallShld, AnimalSize.DogSmallZo)
    val dentalPowderSmall = Product.dentalPowderSmall
    val dentalPowderLarge = Product.dentalPowderLarge

    val boxesToUpdate =
      if (onlyForYesterdayUsers)
        unmodifiedBoxes.filter(_.subscription.obj.exists(_.createdAt.get.isYesterday))
      else
        unmodifiedBoxes

    boxesToUpdate.foreach { box =>
      box.subscriptionItems.toList.foreach(_.delete_!)

      newProducts.map { product =>
        if (!dentalPowder.contains(product))
          SubscriptionItem.createSubscriptionItem(product, box)
        else {
          if (box.fleaTick.obj.map(_.size.get).forall(smallSizes.contains))
            dentalPowderSmall.map(SubscriptionItem.createSubscriptionItem(_, box))
          else
            dentalPowderLarge.map(SubscriptionItem.createSubscriptionItem(_, box))
        }
      }
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
    .withSchedule(CronScheduleBuilder.cronSchedule("0 56 0 ? * * *"))
    .build()
}

object FrequentSubscriptionBoxProductsUpdateJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[SubscriptionBoxProductsUpdateJob])
    .withIdentity("FrequentSubscriptionBoxProductsUpdateJob")
    .build()

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("FrequentSubscriptionBoxProductsUpdateJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */2 * ? * * *"))
    .build()
}
