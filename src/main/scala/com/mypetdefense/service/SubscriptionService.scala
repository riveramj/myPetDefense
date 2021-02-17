package com.mypetdefense.service

import com.mypetdefense.actor.{EmailActor, UpgradeSubscriptionEmail}
import com.mypetdefense.model._
import com.mypetdefense.service.ParentService.updateStripeSubscriptionQuantity
import com.mypetdefense.util.SecurityContext
import net.liftweb.common.Box
import net.liftweb.http.js.JsCmds.Alert
import net.liftweb.util.Helpers.tryo
import net.liftweb.util.Props

import java.time.Month

object SubscriptionService {

  def sameDayCancelsByMonth(subscriptions: List[Subscription]): Map[Month, Int] = {
    val sameDayCancels =
      subscriptions
        .filter(_.status.get == Status.Cancelled)
        .filter(_.filterMailedShipments.isEmpty)

    val cancelsByMonth = sameDayCancels.groupBy { subscription =>
      subscription.getCreatedDateOfSubscription.getMonth
    }

    cancelsByMonth.map {
      case (month, subscriptions) =>
        (month, subscriptions.size)
    }
  }

  def upgradeAccount(updatedSubscription: Box[Subscription], admin: Boolean = false) = {
    val boxes = updatedSubscription.map(_.subscriptionBoxes.toList).openOr(Nil)

    boxes.map(SubscriptionItem.createFirstBox)
    val updatedBoxes = boxes.map(_.reload)

    val cost = updatedBoxes.map(SubscriptionBox.findBoxPrice).sum

    updateStripeSubscriptionQuantity(
      updatedSubscription.map(_.stripeSubscriptionId.get).openOr(""),
      tryo((cost * 100).toInt).openOr(0)
    )

    if (Props.mode == Props.RunModes.Production) {
      EmailActor ! UpgradeSubscriptionEmail(SecurityContext.currentUser, updatedBoxes.size)
    }

    for {
      box          <- updatedBoxes
      subscription <- updatedSubscription.toList
      user         <- SecurityContext.currentUser.toList
      shipmentCount = subscription.shipments.toList.size
    } yield {
      SubscriptionUpgrade.createSubscriptionUpgrade(subscription, box, user, shipmentCount)
      subscription.isUpgraded(true).saveMe()
    }

    if (admin)
      Alert("Account has been upgraded.")
    else
      Alert("Your account has been upgraded! Watch the mail for your new box!")
  }
}
