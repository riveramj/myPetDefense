package com.mypetdefense.jobs

import com.mypetdefense.model._
import com.mypetdefense.service.StripeFacade.Customer
import net.liftweb.mapper._
import org.quartz._

import java.text.SimpleDateFormat

class StripeCouponJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    val dateFormatter = new SimpleDateFormat("M/d/y")
    val startFree     = dateFormatter.parse("8/1/2020")

    for {
      user <- User.findAll(
        By_<(User.createdAt, startFree),
        By(User.referer, Agency.mpdAgency)
      )
    } yield {
      Customer.deleteDiscount(user.stripeId.get)
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
    .withSchedule(CronScheduleBuilder.cronSchedule("0 47 23 ? * * *"))
    .build()
}