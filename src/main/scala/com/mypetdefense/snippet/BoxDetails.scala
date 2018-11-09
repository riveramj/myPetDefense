package com.mypetdefense.snippet

import net.liftweb._
  import http.SHtml._
  import util._
    import Helpers._
  import http._
  import common._
  import sitemap.Menu
  import js._
      import JsCmds._

import com.mypetdefense.service._
    import ValidationService._
    import PetFlowChoices._

import com.mypetdefense._
  import model._
  import snippet.admin.Dashboard
  import snippet.agency.AgencyOverview
import com.mypetdefense.util.{SecurityContext, ClearNodesIf}
import com.mypetdefense.actor._

import me.frmr.stripe.{StripeExecutor, Customer, Coupon => StripeCoupon, Subscription => StripeSubscription}

object BoxDetails extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val thanksgivingBoxMenu = 
    Menu.param[User](
      "box-details", "box-details",
      boxSalesKey => KeyService.findUserByKey(boxSalesKey, "boxSalesKey"),
      user => user.boxSalesKey.get
    ) / "box-details" >>
    MatchWithoutCurrentValue
}

class BoxDetails extends Loggable {
  import BoxDetails._

  var user = BoxDetails.thanksgivingBoxMenu.currentValue

  var exoticCount = 1
  var wellnessCount = 1
  var multivitaminCount = 1

  var cartRenderer: Box[IdMemoizeTransform] = Empty

  user.map(SecurityContext.logIn(_))

  def updateCount(boxName: String, amount: Int, boxRenderer: IdMemoizeTransform) = {
    boxName match {
      case "exotic" =>
        exoticCount = exoticCount + amount
        
        if (exoticCount < 1)
          exoticCount = 1
      case "wellness" =>
        wellnessCount = wellnessCount + amount
        
        if (wellnessCount < 1)
          wellnessCount = 1
      case "multivitamin" =>
        multivitaminCount = multivitaminCount + amount
        
        if (multivitaminCount < 1)
          multivitaminCount = 1
      case _ =>
    }

    boxRenderer.setHtml
  }

  def render = {
    "#shopping-cart" #> idMemoize { renderer =>
      val cart = BoxDetailsFlow.shoppingCart.is

      cartRenderer = Full(renderer)
      
      val subtotal = cart.map { case (quantity, box) =>
        quantity * box.price.get
      }.foldLeft(0D)(_ + _)

      ".cart-item" #> cart.map { case (quantity, box) =>
        val itemPrice = box.price.get * quantity

        ".cart-box-name *" #> box.name.get &
        ".selected-quantity *" #> quantity &
        ".item-price *" #> f"$$$itemPrice%2.2f"
      } &
      ".subtotal *" #> f"$$$subtotal%2.2f"
      //".checkout [href]" #> BoxCheckout.menu.loc.calcDefaultHref
    } andThen
    ".exotic-box .box-quantity-checkout" #> idMemoize { renderer => 
      ".selected-quantity *" #> exoticCount &
      ".subtract [onclick]" #> ajaxInvoke(() => updateCount("exotic", -1, renderer)) &
      ".add [onclick]" #> ajaxInvoke(() => updateCount("exotic", 1, renderer)) 
    } &
    ".wellness-box .box-quantity-checkout" #> idMemoize { renderer =>
      ".selected-quantity *" #> wellnessCount &
      ".subtract [onclick]" #> ajaxInvoke(() => updateCount("wellness", -1, renderer)) &
      ".add [onclick]" #> ajaxInvoke(() => updateCount("wellness", 1, renderer)) 
    } &
    ".multivitamin-box .box-quantity-checkout" #> idMemoize { renderer =>
      ".selected-quantity *" #> multivitaminCount &
      ".subtract [onclick]" #> ajaxInvoke(() => updateCount("multivitamin", -1, renderer)) &
      ".add [onclick]" #> ajaxInvoke(() => updateCount("multivitamin", 1, renderer)) 
    }
  }
}
