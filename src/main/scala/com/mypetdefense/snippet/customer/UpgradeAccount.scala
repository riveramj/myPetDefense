package com.mypetdefense.snippet.customer

import com.mypetdefense.service.SubscriptionService.upgradeAccount
import com.mypetdefense.util.{ClearNodesIf, SecurityContext}
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers._

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
    val user = SecurityContext.currentUser
    val userSubscription = user.flatMap(_.subscription.obj).map(_.reload)

    val updatedSubscription = userSubscription.map(_.promptedUpgrade(true).saveMe())

    val upgraded = updatedSubscription.map(_.isUpgraded.get).openOr(false)

    def doNotUpgradeAccount() = {
      Alert("Your account will not be upgraded. You can upgrade later whenever you would like.")
    }

    SHtml.makeFormsAjax andThen
    ".upgrade-account a [class+]" #> "current" &
    "#user-email *" #> user.map(_.email.get) &
    ".already-upgraded" #> ClearNodesIf(!upgraded) &
    ".upgrade" #> ClearNodesIf(upgraded) andThen
    ".yes-upgrade" #> SHtml.ajaxSubmit("Upgrade me!", () => upgradeAccount(updatedSubscription)) &
    ".no-upgrade" #> SHtml.ajaxSubmit("No thanks.", doNotUpgradeAccount _)
  }
}
