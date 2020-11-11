package com.mypetdefense.snippet.customer

import com.mypetdefense.actor.{EmailActor, UpgradeSubscriptionEmail}
import com.mypetdefense.model._
import com.mypetdefense.service.ParentService.updateStripeSubscriptionQuantity
import com.mypetdefense.util.{ClearNodesIf, SecurityContext}
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers._
import net.liftweb.util.Props

import scala.xml.NodeSeq

object UpgradeAccount extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Upgrade Account") / "upgrade-account" >>
    loggedIn >>
    parent
}

class UpgradeAccount extends Loggable {
  def render: NodeSeq => NodeSeq = {
    val userSubscription =
      SecurityContext.currentUser.flatMap(_.subscription.obj).map(_.reload)

    val updatedSubscription = userSubscription.map(_.promptedUpgrade(true).saveMe())

    val upgraded = updatedSubscription
      .map(_.subscriptionBoxes.toList.map(_.subscriptionItems.toList.nonEmpty))
      .openOr(Nil)
      .foldLeft(true)(_ && _)

    val user = SecurityContext.currentUser

    def upgradeAccount() = {
      val boxes = updatedSubscription.map(_.subscriptionBoxes.toList).openOr(Nil)

      boxes.map(SubscriptionItem.createFirstBox)
      val updatedBoxes = boxes.map(_.reload)

      val cost = updatedBoxes.map(SubscriptionBox.possiblePrice).sum

      updateStripeSubscriptionQuantity(
        user.map(_.stripeId.get).openOr(""),
        updatedSubscription.map(_.stripeSubscriptionId.get).openOr(("")),
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

      Alert("Your account has been upgraded! Watch the mail for your new box!")
    }

    def doNotUpgradeAccount() = {
      Alert("Your account will not be upgraded. You can upgrade later whenever you would like.")
    }

    SHtml.makeFormsAjax andThen
      ".upgrade-account a [class+]" #> "current" &
        "#user-email *" #> user.map(_.email.get) &
        ".already-upgraded" #> ClearNodesIf(!upgraded) &
        ".upgrade" #> ClearNodesIf(upgraded) andThen
      ".yes-upgrade" #> SHtml.ajaxSubmit("Upgrade me!", upgradeAccount _) &
        ".no-upgrade" #> SHtml.ajaxSubmit("No thanks.", doNotUpgradeAccount _)
  }
}
