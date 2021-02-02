package com.mypetdefense.snippet.signup

import com.mypetdefense.service.PetFlowChoices
import com.mypetdefense.service.PetFlowChoices._
import com.mypetdefense.util.Paths.finishedCheckout
import net.liftweb.common._
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

  def render(): NodeSeq => NodeSeq = {
    val pets = petCount.is.openOr(0)
    val monthlyTotal = PetFlowChoices.monthlyTotal.is
    val todayTotal = PetFlowChoices.todayTotal.is

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
