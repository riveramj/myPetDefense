package com.mypetdefense.snippet.signup

import com.mypetdefense.model.User
import com.mypetdefense.service.PetFlowChoices
import com.mypetdefense.service.PetFlowChoices._
import com.mypetdefense.snippet.customer.AccountOverview
import com.mypetdefense.snippet.login.Signup
import com.mypetdefense.util.Paths.finishedCheckout
import com.mypetdefense.util.SecurityContext
import net.liftweb.common._
import net.liftweb.util.ClearNodes
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

object Success extends Loggable {
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Success") / "success" >>
    finishedCheckout
}

class Success extends Loggable {
  purchased(Full(true))
  purchased.is
  val user: Box[User] = SecurityContext.currentUser

  def render(): NodeSeq => NodeSeq = {
    val pets = petCount.is.openOr(0)
    val monthlyTotal = PetFlowChoices.monthlyTotal.is
    val todayTotal = PetFlowChoices.todayTotal.is

    ".next-steps" #>{
      if (needAccountSetup.is.contains(true)) {
        ".view-account" #> ClearNodes &
        ".go-to-account [href]" #> user.map(Signup.menu.toLoc.calcHref)
      } else {
        ".finish-setup" #> ClearNodes &
        ".go-to-account [href]" #> AccountOverview.menu.loc.calcDefaultHref
      }
    } &
    "#order-details" #> {
      "#pet-count span *" #> pets &
      "#monthly-total span *" #> monthlyTotal.map(paid => f"$$$paid%2.2f")
    } & {
      if(todayTotal.map(_ > 0).openOr(false)) {
        "#checkout-total #amount *" #> monthlyTotal.map(paid => f"$$$paid%2.2f")
      } else {
        "#checkout-total *" #> s"First Month Free"
      }
    }
  }
}
