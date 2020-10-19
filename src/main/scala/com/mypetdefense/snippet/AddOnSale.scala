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
import com.mypetdefense.util.{ClearNodesIf, SecurityContext}

import scala.xml.NodeSeq

object AddOnSale extends Loggable {
  import net.liftweb.sitemap._
  import Loc._
  import com.mypetdefense.util.Paths._

  val menu: Menu.Menuable with Menu.WithSlash = Menu.i("Add-on Sale") / "add-on"

  val addOnSaleMenu: Menu.ParamMenuable[User] =
    Menu.param[User](
      "Additional Products",
      "Additional Products",
      productSalesKey => KeyService.findUserByKey(productSalesKey, "productSalesKey"),
      user => user.productSalesKey.get
    ) / "add-on" >>
      MatchWithoutCurrentValue
}

class AddOnSale extends Loggable {
  import AddOnSale._

  var user: Box[User] = AddOnSale.addOnSaleMenu.currentValue

  val duckTreats: Box[Product] =
    Product.find(By(Product.name, "Duck Jerky Multivitamin & Immune Maintenance"))
  val lambTreats: Box[Product] =
    Product.find(By(Product.name, "Lamb Jerky Digestive Health & Probiotic"))
  val beefTreats: Box[Product] = Product.find(By(Product.name, "Beef Jerky Hip & Joint Formula"))
  val salmonTreats: Box[Product] =
    Product.find(By(Product.name, "Salmon Jerky Skin & Coat Formula"))
  val fruitTreats: Box[Product] =
    Product.find(By(Product.name, "Healthy Harvest Fruit and Veggie Mix"))

  var cartRenderer: Box[IdMemoizeTransform] = Empty

  user.map(SecurityContext.logIn(_))

  def updateCartCount(treat: Product, newQuantity: Int): JsCmd = {
    val cart = AddOnFlow.addOnShoppingCart.is

    val updatedCart = {
      if (newQuantity < 1)
        cart - treat
      else
        cart + (treat -> newQuantity)
    }

    AddOnFlow.addOnShoppingCart(updatedCart)

    cartRenderer.map(_.setHtml).openOr(Noop)
  }

  def addToCart(possibleTreat: Box[Product]): JsCmd = {
    possibleTreat.map { treat =>
      val cart = AddOnFlow.addOnShoppingCart.is

      val updatedCart = {
        if (cart.contains(treat))
          cart + (treat -> (cart(treat) + 1))
        else
          cart + (treat -> 1)
      }

      AddOnFlow.addOnShoppingCart(updatedCart)
    }

    cartRenderer.map(_.setHtml).openOr(Noop)
  }

  def removeTreatFromCart(treat: Product): JsCmd = {
    val cart = AddOnFlow.addOnShoppingCart.is

    AddOnFlow.addOnShoppingCart(cart - treat)

    cartRenderer.map(_.setHtml).openOr(Noop)
  }

  def render: NodeSeq => NodeSeq = {
    "#logo-name a [href]" #> AddOnSale.addOnSaleMenu.loc.calcDefaultHref &
      "#shopping-cart" #> idMemoize { renderer =>
        val cart = AddOnFlow.addOnShoppingCart.is

        cartRenderer = Full(renderer)

        val subtotal = cart.map {
          case (treat, quantity) =>
            quantity * treat.price.get
        }.foldLeft(0d)(_ + _)

        ".items-in-cart .cart-item" #> cart.map {
          case (treat, quantity) =>
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
              ".checkout [href]" #> AddOnCheckout.menu.loc.calcDefaultHref
          } &
          ".items-in-cart .subtotal-container .subtotal *" #> f"$$$subtotal%2.2f" &
          ".cart-actions .checkout [href]" #> AddOnCheckout.menu.loc.calcDefaultHref &
          ".items-in-cart" #> ClearNodesIf(cart.isEmpty) &
          ".cart-footer" #> ClearNodesIf(cart.isEmpty) &
          ".cart-actions" #> ClearNodesIf(cart.isEmpty) &
          ".empty-cart" #> ClearNodesIf(!cart.isEmpty)
      } andThen
      ".duck .add-treat [onclick]" #> ajaxInvoke(() => addToCart(duckTreats)) &
        ".lamb .add-treat [onclick]" #> ajaxInvoke(() => addToCart(lambTreats)) &
        ".beef .add-treat [onclick]" #> ajaxInvoke(() => addToCart(beefTreats)) &
        ".salmon .add-treat [onclick]" #> ajaxInvoke(() => addToCart(salmonTreats)) &
        ".fruit-small .add-treat [onclick]" #> ajaxInvoke(() => addToCart(fruitTreats))
  }
}
