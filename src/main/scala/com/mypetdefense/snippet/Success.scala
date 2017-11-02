package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
import net.liftweb.mapper.{By, NullRef}

import com.mypetdefense.service.PetFlowChoices._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.model._

import scala.collection.mutable.LinkedHashMap

object Success extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Success") / "success"
}

class Success extends Loggable {
  purchased(Full(true))
  purchased.is

  def render() = {
    val petCount = shoppingCart.is.size
    val monthylTotal = total.is
    val freeMonthCount = freeMonths.is.openOr(0)

    val subscriptionLength = groupons.headOption.map(_.freeMonths.get).getOrElse(0)

    "#count span *" #> petCount &
    "#subscription-length" #> ClearNodesIf(groupons.isEmpty) &
    "#subscription-length span *" #> s"${subscriptionLength} months" &
    "#monthly-total" #> ClearNodesIf(freeMonthCount == 0) &
    "#monthly-total span *" #> monthylTotal.map( paid => f"$$$paid%2.2f" ) &
    {
      if (freeMonthCount == 0 || !groupons.isEmpty) {
        "#checkout-total #amount *" #> monthylTotal.map( paid => f"$$$paid%2.2f" )
      } else if (freeMonthCount == 1) {
        "#checkout-total *" #> s"First Month Free"
      } else {
        "#checkout-total *" #> s"First ${freeMonthCount} months free"
      }
    }
  }
}
