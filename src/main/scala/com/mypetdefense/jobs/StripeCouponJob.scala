package com.mypetdefense.jobs

import com.mypetdefense.model._
import net.liftweb.mapper._
import org.quartz._
import com.stripe.{model => StripeModel}
import net.liftweb.common.Box

import java.text.SimpleDateFormat

class StripeCouponJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    val dateFormatter = new SimpleDateFormat("M/d/y")
    val startFree     = dateFormatter.parse("8/1/2020")

    for {
      subscription <- Subscription.findAll(
        By_<(Subscription.startDate, startFree),
        By(Subscription.isUpgraded, true)
      )
      stripeSubscription <- Box.tryo(StripeModel.Subscription.retrieve(subscription.stripeSubscriptionId.get))
    } yield {
      Box.tryo(stripeSubscription.deleteDiscount())
    }
  }
}

object DailyStripeCouponJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[StripeCouponJob])
    .withIdentity("DailyStripeCouponJob")
    .build()

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("DailyStripeCouponJob")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 52 23 ? * * *"))
    .build()
}