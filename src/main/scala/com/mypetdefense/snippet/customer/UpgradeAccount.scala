package com.mypetdefense.snippet.customer

import java.text.SimpleDateFormat
import java.time.{LocalDate, ZoneId}
import java.util.Date

import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.service.ParentService.updateStripeSubscriptionQuantity
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import com.mypetdefense.util.SecurityContext._
import com.mypetdefense.util.{ClearNodesIf, SecurityContext}
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers._
import net.liftweb.util._

object UpgradeAccount extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu = Menu.i("Upgrade Account") / "upgrade-account" >>
    loggedIn >>
    parent
}

class UpgradeAccount extends Loggable {
  def render = {
    val userSubscription = SecurityContext.currentUser.flatMap(_.subscription.obj).flatMap(_.refresh)

    val updatedSubscription = userSubscription.map(_.promptedUpgrade(true).saveMe())

    val upgraded = updatedSubscription.map(_.subscriptionBoxes.toList.map(_.subscriptionItems.toList.nonEmpty)).openOr(Nil).foldLeft(true)(_&&_)

    def upgradeAccount() = {

      val boxes = updatedSubscription.map(_.subscriptionBoxes.toList).openOr(Nil)
      val user = SecurityContext.currentUser
      boxes.map(SubscriptionItem.createFirstBox)
      val updatedBoxes = boxes.flatMap(_.refresh)

      ParentService.updateCoupon(user.map(_.stripeId.get).openOr(""), Full("upgrade01"))

      val cost = updatedBoxes.map(SubscriptionBox.possiblePrice).sum

      updateStripeSubscriptionQuantity(
        user.map(_.stripeId.get).openOr(""),
        updatedSubscription.map(_.stripeSubscriptionId.get).openOr(("")),
        tryo((cost * 100).toInt).openOr(0)
      )

      Alert("Your account has been upgraded! Watch the mail for your new box!")
    }

    def doNotUpgradeAccount() = {
      Alert("Your account will not be upgraded. You can upgrade later whenever you would like.")
    }

    SHtml.makeFormsAjax andThen
    ".upgrade-account a [class+]" #> "current" &
    ".already-upgraded" #> ClearNodesIf(!upgraded) &
    ".upgrade" #> ClearNodesIf(upgraded) andThen
    ".yes-upgrade" #> SHtml.ajaxSubmit("Upgrade me!", upgradeAccount _) &
    ".no-upgrade" #> SHtml.ajaxSubmit("No thanks.", doNotUpgradeAccount _)
  }
}
