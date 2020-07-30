package com.mypetdefense.snippet.shop

import com.mypetdefense._
import com.mypetdefense.model._
import com.mypetdefense.service._
import com.mypetdefense.util.{ClearNodesIf, SecurityContext}
import net.liftweb._
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._
import net.liftweb.util._

object TreatList extends Loggable {
  import net.liftweb.sitemap._
  import Loc._

  val menu = Menu.i("Treats") / "treats"

  val treatListMenu =
    Menu.param[User](
      "Our Treats", "Our Treats",
      productSalesKey => KeyService.findUserByKey(productSalesKey, "productSalesKey"),
      user => user.productSalesKey.get
    ) / "treats" >>
    MatchWithoutCurrentValue
}

class TreatList extends Loggable {

  var user = TreatList.treatListMenu.currentValue

  val duckTreats = Product.find(By(Product.name, "Duck Jerky Multivitamin & Immune Maintenance"))
  val lambTreats = Product.find(By(Product.name, "Lamb Jerky Digestive Health & Probiotic"))
  val beefTreats = Product.find(By(Product.name, "Beef Jerky Hip & Joint Formula"))
  val salmonTreats = Product.find(By(Product.name, "Salmon Jerky Skin & Coat Formula"))
  val fruitTreats = Product.find(By(Product.name, "Mind Your Peas Natural Dog Treats"))

  var cartRenderer: Box[IdMemoizeTransform] = Empty

  user.map(SecurityContext.logIn(_))

  def updateCartCount(treat: Product, newQuantity: Int) = {
    val cart = TreatsFlow.treatShoppingCart.is

    val updatedCart = {
      if (newQuantity < 1)
        cart - treat
      else
        cart + (treat -> newQuantity)
    }

    TreatsFlow.treatShoppingCart(updatedCart)

    cartRenderer.map(_.setHtml).openOr(Noop)
  }

  def addToCart(possibleTreat: Box[Product]) = {
    possibleTreat.map { treat =>
      val cart = TreatsFlow.treatShoppingCart.is

      val updatedCart = {
        if (cart.contains(treat))
          cart + (treat -> (cart(treat) + 1))
        else
          cart + (treat -> 1)
      }

      TreatsFlow.treatShoppingCart(updatedCart)
    }

    cartRenderer.map(_.setHtml).openOr(Noop)
  }

  def removeTreatFromCart(treat: Product) = {
    val cart = TreatsFlow.treatShoppingCart.is

    TreatsFlow.treatShoppingCart(cart - treat)

    cartRenderer.map(_.setHtml).openOr(Noop)
  }

  def render = {
    "#logo-name a [href]" #> TreatList.treatListMenu.loc.calcDefaultHref &
    "#shopping-cart" #> idMemoize { renderer =>
      val cart = TreatsFlow.treatShoppingCart.is

      cartRenderer = Full(renderer)

      val subtotal = cart.map { case (treat, quantity) =>
        quantity * treat.price.get
      }.foldLeft(0D)(_ + _)

      ".items-in-cart .cart-item" #> cart.map { case (treat, quantity) =>
        val itemPrice = treat.price.get * quantity

        ".cart-treat-name *" #> treat.name.get &
        ".selected-quantity *" #> quantity &
        ".remove-treat [onclick]" #> ajaxInvoke(() => removeTreatFromCart(treat)) &
        ".subtract [onclick]" #> ajaxInvoke(() => updateCartCount(treat, quantity - 1)) &
        ".add [onclick]" #> ajaxInvoke(() => updateCartCount(treat, quantity + 1)) &
        ".treat-price *" #> f"$$$itemPrice%2.2f"
      } &
      ".cart-footer" #> {
        ".subtotal *" #> f"$$$subtotal%2.2f" &
        ".checkout [href]" #> TreatCheckout.menu.loc.calcDefaultHref
      } &
      ".items-in-cart .subtotal-container .subtotal *" #> f"$$$subtotal%2.2f" &
      ".cart-actions .checkout [href]" #> TreatCheckout.menu.loc.calcDefaultHref &
      ".items-in-cart" #> ClearNodesIf(cart.isEmpty) &
      ".cart-footer" #> ClearNodesIf(cart.isEmpty) &
      ".cart-actions" #> ClearNodesIf(cart.isEmpty) &
      ".empty-cart" #> ClearNodesIf(!cart.isEmpty)
    } andThen
    ".fruit .add-treat [onclick]" #> ajaxInvoke(() => addToCart(fruitTreats))
  }
}
