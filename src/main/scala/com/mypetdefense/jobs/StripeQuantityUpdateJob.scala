package com.mypetdefense.jobs

import com.mypetdefense.model.{BoxType, Status, Subscription, SubscriptionBox}
import com.mypetdefense.util.StripeHelper.{ParamsMap, StripeParamOps}
import net.liftweb.common.Box
import net.liftweb.common.Box.tryo
import net.liftweb.mapper.By
import org.quartz._
import com.stripe.model.{Subscription => StripeSubscription}

class StripeQuantityUpdateJob extends ManagedJob {
  override def execute(context: JobExecutionContext): Unit = {
    println("Starting Job")
    val totalMissingRevenue = (for {
      subscription <- Subscription.findAll(
        By(Subscription.status, Status.Active),
        By(Subscription.isUpgraded, true)
      )
      stripeSubscription <- Box.tryo(StripeSubscription.retrieve(subscription.stripeSubscriptionId.get))
      if stripeSubscription.getQuantity == 0
      user <- subscription.user.obj
      shipments = subscription.shipments.toList
      boxes = subscription.subscriptionBoxes
    } yield {
      val basePrice = (for {
        box <- boxes
        pet <- box.pet.obj
      } yield {
        SubscriptionBox.basePrice(pet, box.boxType.get == BoxType.healthAndWellness)
      }).sum

      val pennyCount = tryo((basePrice * 100).toInt)

      val shipmentPricesPaid = shipments.map(_.amountPaid.get)
      val zeroPaidShipments = shipments.filter(_.amountPaid.get == "0" )
      val nonFreeBoxes = zeroPaidShipments.size - 1
      val missingRevenue = if (zeroPaidShipments.size > 1)
        shipments.tail.size * basePrice
      else 0D

      println("0 Amount Subscription")

      println(user.email.get)
      println(subscription.stripeSubscriptionId.get)
      println("$" + pennyCount.openOr(-1) + "subscription value")
      println("number of non free boxes: " + nonFreeBoxes)
      println("prices paid for shipments: " + shipmentPricesPaid)
      println("missing revenue: " + missingRevenue)
      println("=====================")

      val params = ParamsMap(
        "quantity" --> pennyCount.openOr(0),
        "prorate" --> false
      )

      //StripeHelper.updateSubscription(subscription.stripeSubscriptionId.get, params)
      missingRevenue
    }).sum

    println("missing revenue: " + totalMissingRevenue)
    println("End Job")
  }
}

object OnDemandStripeQuantityUpdateJob extends TriggeredJob {
  override def detail: JobDetail =
    JobBuilder
      .newJob(classOf[StripeQuantityUpdateJob])
      .withIdentity("OnDemandStripeQuantityUpdateJob")
      .build()

  override def trigger: Trigger =
    TriggerBuilder
      .newTrigger()
      .withIdentity("OnDemandStripeQuantityUpdateJob")
      .startNow()
      .withSchedule(CronScheduleBuilder.cronSchedule("0 44 15 ? * * *")) // At 03:00:00am every day
      .build()
}
