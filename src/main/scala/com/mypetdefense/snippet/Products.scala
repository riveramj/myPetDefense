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


  def render = {
    ".frontline-dogs .month-price *" #> getPriceForProduct("Frontline", "Dogs") &
    ".zoguard-dogs .month-price *" #> getPriceForProduct("ZoGuard", "Dogs") &
    ".adventure-dogs .month-price *" #> getPriceForProduct("Adventure", "Dogs") &
    ".shieldtec-dogs .month-price *" #> getPriceForProduct("ShieldTec", "Dogs") &
    ".frontline-cats .month-price *" #> getPriceForProduct("Frontline", "Cats") &
    ".zoguard-cats .month-price *" #> getPriceForProduct("ZoGuard", "Cats") &
    ".adventure-cats .month-price *" #> getPriceForProduct("Adventure", "Cats")
  }
}
