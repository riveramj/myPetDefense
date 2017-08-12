package com.mypetdefense.snippet 

import net.liftweb.sitemap.Menu
import net.liftweb._
  import http.SHtml._
  import util._
  import util.Helpers._
  import common._
  import util.ClearClearable
  import http._
  import mapper.{By, NullRef}
  import js._
      import JsCmds._

import com.mypetdefense.service._
  import ValidationService._
  import PetFlowChoices._

import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.model._
import com.mypetdefense.actor._
import com.mypetdefense.util.RandomIdGenerator._

object ProductDetail extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val frontlineDogsMenu = Menu.i("Frontline Plus for Dogs") / "frontline-dog-detail"
  val zoguardDogsMenu = Menu.i("ZoGuard Plus for Dogs") / "zoguard-dog-detail"
  val adventureDogsMenu = Menu.i("Adventure Plus for Dogs") / "adventure-dog-detail"
  val sheieldtecDogsMenu = Menu.i("ShieldTec Plus for Dogs") / "shieldtec-dog-detail"
  val frontlineCatsMenu = Menu.i("Frontline Plus for Cats") / "frontline-cat-detail"
  val zoguardCatsMenu = Menu.i("ZoGuard Plus for Cats") / "zoguard-cat-detail"
  val adventureCatsMenu = Menu.i("Adventure Plus for Cats") / "adventure-cat-detail"

}

class ProductDetail extends Loggable {
  import PetFlowChoices._

  val path = S.request.map(_.uri).openOr("").drop(1)

  var cartRenderer: Box[IdMemoizeTransform] = Empty

  val products = path match {
    case "frontline-dog-detail" => Product.findAll(By(Product.name, "Frontline Plus for Dogs"))
    case "adventure-dog-detail" => Product.findAll(By(Product.name, "Adventure Plus for Dogs"))
    case "shieldtec-dog-detail" => Product.findAll(By(Product.name, "ShieldTec Plus for Dogs"))
    case "zoguard-dog-detail" => Product.findAll(By(Product.name, "ZoGuard Plus for Dogs"))
    case "frontline-cat-detail" => Product.findAll(By(Product.name, "Frontline Plus for Cats"))
    case "adventure-cat-detail" => Product.findAll(By(Product.name, "Adventure Plus for Cats"))
    case "zoguard-cat-detail" => Product.findAll(By(Product.name, "ZoGuard Plus for Cats"))
  }

  def addToCart(product: Product, name: String, price: Double) = {
    recentProduct(Full(product))
    shoppingCart(shoppingCart.is + (generateLongId -> (name, product, price)))
    cartRenderer.map(_.setHtml).openOr(Noop)
  }

  def getImageUrl(product: Box[Product]) = {
    s"images/product-shots/${product.map(_.imageName).openOr("")}"
  }

  def render = {
    SHtml.makeFormsAjax andThen
    "#switch-save" #> ClearNodesIf(!path.contains("frontline")) &
    ".product-name *" #> products.headOption.map(_.name.get).getOrElse("") &
    ".product" #> products.sortWith(_.size.get < _.size.get).map { product =>
      val price = Price.getDefaultProductPrice(product).map(_.price.get).openOr(0D)
      var name = ""

      ".size *" #> s"${product.getSizeAndSizeName}" &
      ".price *" #> f"$$$price%2.2f" &
      ".pet-name" #> ajaxText("", name = _) &
      ".add-to-cart [onclick]" #> ajaxInvoke(() => addToCart(product, name, price))
    } & 
    "#shopping-cart" #> idMemoize { renderer =>
      val cart = shoppingCart.is

      cartRenderer = Full(renderer)
      val subtotal = cart.values.map(_._3).sum
      val multiPetDiscount = cart.size match {
        case 0 | 1 => 0
        case 2 => subtotal * 0.05
        case _ => subtotal * 0.1
      }

      val subtotalWithDiscount = subtotal - multiPetDiscount
      
      ".added-product *" #> recentProduct.map(_.getNameAndSize).openOr("") &
      ".added-product-image [src]" #> getImageUrl(recentProduct) &
      ".cart-item" #> cart.values.map { cartItem =>
        val itemPrice = cartItem._3

        ".cart-product-image [src]" #> getImageUrl(Full(cartItem._2)) &
        ".cart-pet-name *" #> cartItem._1 &
        ".cart-pet-price *" #> f"$$$itemPrice%2.2f"
      } &
      ".discount *" #> f"-$$$multiPetDiscount%2.2f" &
      ".subtotal *" #> f"$$$subtotalWithDiscount%2.2f" &
      ".checkout [href]" #> CartReview.menu.loc.calcDefaultHref
    }
  }
}
