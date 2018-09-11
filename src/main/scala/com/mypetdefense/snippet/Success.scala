package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearNodes
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
    if (boxSalesInfo.is != Empty) {
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
      "#box-count" #> ClearNodes &
      "#box-total" #> ClearNodes
    } else {
      val boxQuantity = boxSalesInfo.is.map(_._1)
      val boxSalesTotal = boxSalesInfo.is.map(_._2).openOr(0D)

      "#order-summary [class+]" #> "box-sale" &
      "#order-total h3 [class+]" #> "box-sale" andThen
      "#box-count span *" #> boxQuantity &
      "#monthly-total span *" #> f"$$$boxSalesTotal%2.2f" &
      "#pet-count" #> ClearNodes &
      "#monthly-total" #> ClearNodes &
      "#checkout-total .per-month" #> ClearNodes
    }
  }
}
