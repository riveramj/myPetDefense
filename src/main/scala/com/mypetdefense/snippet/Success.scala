package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearNodes
import net.liftweb.http._
import net.liftweb.mapper.{By, NullRef}

import com.mypetdefense.service.PetFlowChoices._
import com.mypetdefense.service.TreatsFlow

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
    if (TreatsFlow.treatSale.is == Empty) {
      val petCount = shoppingCart.is.size
      val monthylTotal = total.is
      val freeMonthCount = freeMonths.is.openOr(0)

      "#pet-count span *" #> petCount &
      "#monthly-total" #> ClearNodesIf(freeMonthCount == 0) &
      "#monthly-total span *" #> monthylTotal.map( paid => f"$$$paid%2.2f" ) &
      {
        if (freeMonthCount == 0) {
          "#checkout-total #amount *" #> monthylTotal.map( paid => f"$$$paid%2.2f" )
        } else if (freeMonthCount == 1) {
          "#checkout-total *" #> s"First Month Free"
        } else {
          "#checkout-total *" #> s"First ${freeMonthCount} months free"
        }
      } &
      "treat-sold" #> ClearNodes
    } else {
      val treatSalesTotal = TreatsFlow.treatSale.is.map(_._1).openOr(0D)
      val treats = TreatsFlow.treatSale.is.map(_._2).openOr(Map())

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
