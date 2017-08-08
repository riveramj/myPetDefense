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
}

class ProductDetail extends Loggable {
  import PetFlowChoices._

  val path = S.request.map(_.uri).openOr("").drop(1)

  var cartRenderer: Box[IdMemoizeTransform] = Empty
  var name = ""

  val products = path match {
    case "frontline-dog-detail" => Product.findAll(By(Product.name, "Frontline Plus for Dogs"))
  }

  def addToCart(product: Product, name: String, price: Double)() = {
    shoppingCart(shoppingCart.is :+ (generateLongId, name, product, price))
    cartRenderer.map(_.setHtml).openOr(Noop)
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".product" #> products.map { product =>
      val price = Price.getDefaultProductPrice(product).map(_.price.get).openOr(0D)

      ".size *" #> s"${product.getSizeAndSizeName}" &
      ".price *" #> s"$$${price}" &
      ".pet-name" #> ajaxText("", name = _) &
      ".add-to-cart [onclick]" #> ajaxInvoke(addToCart(product, name, price))
    } & 
    "#items-in-cart" #> idMemoize { renderer =>
      cartRenderer = Full(renderer)

      ".cart-item" #> shoppingCart.is.map { cartItem =>
        println(cartItem)

        ".cart-pet-name *" #> cartItem._2 &
        ".cart-pet-price *" #> s"$$${cartItem._4}"
      }
    }
  }
}
