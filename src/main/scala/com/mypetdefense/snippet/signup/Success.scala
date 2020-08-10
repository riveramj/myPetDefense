package com.mypetdefense.snippet.signup

import com.mypetdefense.service.PetFlowChoices._
import com.mypetdefense.service.TreatsFlow
import com.mypetdefense.util.ClearNodesIf
import net.liftweb.common._
import net.liftweb.util.ClearNodes
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

object Success extends Loggable {
  import net.liftweb.sitemap._

  val menu: Menu.Menuable with Menu.WithSlash = Menu.i("Success") / "success"
}

class Success extends Loggable {
  purchased(Full(true))
  purchased.is

  def render(): NodeSeq => NodeSeq = {
    if (TreatsFlow.treatShoppingCart.is == Map()) {
      val pets = petCount.is.openOr(0)
      val monthlyTotal = total.is
      val freeMonthCount = freeMonths.is.openOr(0)

      "#order-details" #> {
        "#pet-count span *" #> pets &
        "#monthly-total" #> ClearNodesIf(freeMonthCount == 0) &
        ".treat-sold" #> ClearNodes &
        "#monthly-total span *" #> monthlyTotal.map( paid => f"$$$paid%2.2f" )
      } &
      {
        if (freeMonthCount == 0) {
          "#checkout-total #amount *" #> monthlyTotal.map( paid => f"$$$paid%2.2f" )
        } else if (freeMonthCount == 1) {
          "#checkout-total *" #> s"First Month Free"
        } else {
          "#checkout-total *" #> s"First ${freeMonthCount} months free"
        }
      }
    } else {
      val treatSalesTotal = TreatsFlow.treatSale.is.map(_._1).openOr(0D)
      val treats = TreatsFlow.treatSale.is.map(_._2).openOr(Map())

      TreatsFlow.treatSale(Empty)
      TreatsFlow.treatShoppingCart(Map())

      "#order-summary [class+]" #> "treat-sale" &
      "#order-total h3 [class+]" #> "treat-sale" andThen
      "#order-details .treat-sold" #> treats.map { case (treat, count) =>
        ".treat-name *" #> treat.name &
        ".treat-count *" #> count
      } &
      "#checkout-total #amount *" #> f"$$$treatSalesTotal%2.2f" &
      "#order-details" #> {
        "#pet-count" #> ClearNodes &
        "#monthly-total" #> ClearNodes
      } &
      "#checkout-total .per-month" #> ClearNodes
    }
  }
}
