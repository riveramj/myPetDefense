package com.mypetdefense.jobs

import com.mypetdefense.actor.{EmailActor, SendPreBillingEmail}
import com.mypetdefense.model.Subscription.findSubscriptionsShippingInThreeDays
import com.mypetdefense.service.KeyService
import net.liftweb.json.DefaultFormats
import org.quartz._

class PreBillingEmailJob extends ManagedJob {
  implicit val formats: DefaultFormats.type = DefaultFormats

  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    for {
      subscription <- findSubscriptionsShippingInThreeDays
      user <- subscription.user.obj
      boxes = subscription.subscriptionBoxes.toList
      address <- user.addresses.headOption
      _ = KeyService.generateNewPreBillingKey(user)
    } {
      EmailActor ! SendPreBillingEmail(
        user.reload,
        address,
        subscription,
        boxes
      )
    }
  }
}

object DailyPreBillingEmailJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[PreBillingEmailJob])
    .withIdentity("PreBillingEmailJob")
    .build()

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("DailyPreBillingEmailJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */5 0-8 ? * * *"))
    .build()
}

object FrequentPreBillingEmailJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[PreBillingEmailJob])
    .withIdentity("PreBillingEmailJobJob")
    .build

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("FrequentPreBillingEmailJobTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 5 minutes
    .build
}
