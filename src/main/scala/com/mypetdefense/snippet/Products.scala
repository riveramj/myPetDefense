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

object Products extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Products") / "products"
}

class Products extends Loggable {
  PetFlowChoices.priceCode(Full("default"))
  val products = Product.findAll()

  val prices = Price.findAll(
    By(Price.code, "default"),
    By(Price.active, true)
  )

  def getPriceForProduct(product: String, petType: String) = {
    val price = prices.filter { price =>
      val productName = price.product.obj.map(_.name.get).getOrElse("")
      productName == s"${product} Plus for ${petType}"
    }.headOption.map(_.price.get).getOrElse(0D)

    f"$$$price%2.2f"
  }

  def getImageUrl(product: Box[Product]) = {
    s"images/product-shots/${product.map(_.imageName).openOr("")}"
  }
  
  def getImgeAndPrice(productName: String): (String, Double) = {
    val product = products.filter(_.name.get == productName).headOption
    val imageName = s"images/product-shots/${product.map(_.imageName.get).getOrElse("")}"
    val price: Double = product.flatMap { possibleProduct =>
      Price.getDefaultProductPrice(possibleProduct).map(_.price.get)
    }.getOrElse(0D)

    (imageName, price)
  }

  def render = {
    ".frontline-dogs" #> {
      val (imageName, price) = getImgeAndPrice("Frontline Plus for Dogs")

      ".product-shot img [src]" #> imageName &
      ".month-price *" #> f"$$$price%2.2f"
    } &
    ".zoguard-dogs" #> {
      val (imageName, price) = getImgeAndPrice("ZoGuard Plus for Dogs")

      ".product-shot img [src]" #> imageName &
      ".month-price *" #> f"$$$price%2.2f"
    } &
    ".adventure-dogs" #> {
      val (imageName, price) = getImgeAndPrice("Adventure Plus for Dogs")

      ".product-shot img [src]" #> imageName &
      ".month-price *" #> f"$$$price%2.2f"
    } &
    ".shieldtec-dogs" #> {
      val (imageName, price) = getImgeAndPrice("ShieldTec Plus for Dogs")

      ".product-shot img [src]" #> imageName &
      ".month-price *" #> f"$$$price%2.2f"
    } &
    ".frontline-cats" #> {
      val (imageName, price) = getImgeAndPrice("Frontline Plus for Cats")

      ".product-shot img [src]" #> imageName &
      ".month-price *" #> f"$$$price%2.2f"
    } &
    ".zoguard-cats" #> {
      val (imageName, price) = getImgeAndPrice("ZoGuard Plus for Cats")

      ".product-shot img [src]" #> imageName &
      ".month-price *" #> f"$$$price%2.2f"
    } &
    ".adventure-cats" #> {
      val (imageName, price) = getImgeAndPrice("Adventure Plus for Cats")

      ".product-shot img [src]" #> imageName &
      ".month-price *" #> f"$$$price%2.2f"
    }
  }
}
