package com.mypetdefense.snippet

import net.liftweb._
  import http.SHtml._
  import util._
    import Helpers._
  import http._
  import mapper.By
  import common._
  import sitemap.Menu
  import js._
      import JsCmds._

import com.mypetdefense.service._

import com.mypetdefense._
  import model._
import com.mypetdefense.util.SecurityContext

object TreatList extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val treatListMenu = 
    Menu.param[User](
      "Our Treats", "Our Treats",
      productSalesKey => KeyService.findUserByKey(productSalesKey, "productSalesKey"),
      user => user.productSalesKey.get
    ) / "treat-list" >>
    MatchWithoutCurrentValue
}

class TreatList extends Loggable {
  import TreatList._

  var user = TreatList.treatListMenu.currentValue
  
  val duckTreats = Treat.find(By(Treat.name, "Duck Jerky Multivitamin & Immune Maintenance"))
  val lambTreats = Treat.find(By(Treat.name, "Lamb Jerky Digestive Health & Probiotic"))
  val beefTreats = Treat.find(By(Treat.name, "Beef Jerky Hip & Joint Formula"))
  val chickenTreats = Treat.find(By(Treat.name, "Chicken Jerky Skin & Coat Formula"))
  
  var cartRenderer: Box[IdMemoizeTransform] = Empty

  user.map(SecurityContext.logIn(_))

  def updateCartCount(treat: Treat, newQuantity: Int) = {
    val cart = TreatsFlow.shoppingCart.is

    val updatedCart = {
      if (newQuantity < 1)
        cart - treat
      else
        cart + (treat -> newQuantity)
    }

    TreatsFlow.shoppingCart(updatedCart)

    cartRenderer.map(_.setHtml).openOr(Noop)
  }

  def addToCart(possibleTreat: Box[Treat]) = {
    possibleTreat.map { treat =>
      val cart = TreatsFlow.shoppingCart.is

      val updatedCart = {
        if (cart.contains(treat))
          cart + (treat -> (cart(treat) + 1))
        else
          cart + (treat -> 1)
      }

      TreatsFlow.shoppingCart(updatedCart)
    }
    
    cartRenderer.map(_.setHtml).openOr(Noop)
  }

  def removeTreatFromCart(treat: Treat) = {
    val cart = TreatsFlow.shoppingCart.is

    TreatsFlow.shoppingCart(cart - treat)

    cartRenderer.map(_.setHtml).openOr(Noop)
  }

  def render = {
    "#logo-name a [href]" #> TreatList.treatListMenu.loc.calcDefaultHref &
    "#shopping-cart" #> idMemoize { renderer =>
      val cart = TreatsFlow.shoppingCart.is

      cartRenderer = Full(renderer)
      
      val subtotal = cart.map { case (treat, quantity) =>
        quantity * treat.price.get
      }.foldLeft(0D)(_ + _)

      ".cart-item" #> cart.map { case (treat, quantity) =>
        val itemPrice = treat.price.get * quantity

        ".cart-treat-name *" #> treat.name.get &
        ".selected-quantity *" #> quantity &
        ".remove-treat [onclick]" #> ajaxInvoke(() => removeTreatFromCart(treat)) &
        ".subtract [onclick]" #> ajaxInvoke(() => updateCartCount(treat, quantity - 1)) &
        ".add [onclick]" #> ajaxInvoke(() => updateCartCount(treat, quantity + 1)) &
        ".item-price *" #> f"$$$itemPrice%2.2f"
      } &
      ".subtotal *" #> f"$$$subtotal%2.2f" &
      ".checkout [href]" #> TreatCheckout.menu.loc.calcDefaultHref
    } andThen
    ".duck .add-treat [onclick]" #> ajaxInvoke(() => addToCart(duckTreats)) &
    ".lamb .add-treat [onclick]" #> ajaxInvoke(() => addToCart(lambTreats)) &
    ".beef .add-treat [onclick]" #> ajaxInvoke(() => addToCart(beefTreats)) &
    ".chicken .add-treat [onclick]" #> ajaxInvoke(() => addToCart(chickenTreats))
  }
}
