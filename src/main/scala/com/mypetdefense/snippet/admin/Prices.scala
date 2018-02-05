package com.mypetdefense.snippet 
package admin

import net.liftweb._
  import sitemap.Menu
  import http.SHtml._
  import http._
  import js.JsCmds._

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.service._
  import ValidationService._
import com.mypetdefense.actor._

import com.mypetdefense.util.RandomIdGenerator._

object Prices extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Prices") / "admin" / "prices" >>
    adminUser >>
    loggedIn
}

case class DisplayPrice(code: String, price: Double, productName: String)

class Prices extends Loggable {
  val productNames = Product.findAll().map(_.name.get).distinct
  val allPrices = Price.findAll()
  val prices: List[DisplayPrice] = allPrices.map { price =>
    DisplayPrice(price.code.get, price.price.get, price.product.obj.map(_.name.get).openOr(""))
  }.distinct

  var code = ""
  var rawPrice = ""
  var chosenProduct = ""
  
  def productDropdown = {
    SHtml.select(
      ("","") +: productNames.sortWith(_<_).map(name => (name,name)),
      Full(chosenProduct),
      chosenProduct = _
    )
  }

  def createPrice = {
    val price = tryo(rawPrice.trim().toDouble).getOrElse(0D)
    val validateFields = List(
      checkEmpty(code, "#code"),
      checkEmpty(price.toString, "#price")
    ).flatten

    if(validateFields.isEmpty) {
      val priceDbId = generateLongId
      val date = LocalDate.now
      val name = s"${chosenProduct} (${code} ${date})"

      val selectedProducts = Product.findAll(By(Product.name, chosenProduct))
      for {
        product <- selectedProducts
      } yield {
        Price.createPrice(priceDbId, price, code, product, name)
      }

      S.redirectTo(Prices.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def deletePrice(price: DisplayPrice)() = {
    val products = Product.findAll(By(Product.name, price.productName))
    products.map { product =>
      val prices = Price.findAll(By(Price.product, product), By(Price.code, price.code))
      prices.map(_.delete_!)
    }
    
    S.redirectTo(Prices.menu.loc.calcDefaultHref)
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".prices [class+]" #> "current" &
    ".create" #> idMemoize { renderer =>
      "#code" #> ajaxText(code, code = _) &
      "#price" #> ajaxText(rawPrice, rawPrice = _) &
      "#product-container #product-select" #> productDropdown &
      "#create-item" #> SHtml.ajaxSubmit("Create Price", () => createPrice)
    } &
    ".price-entry" #> prices.sortWith(_.code < _.code).map { price =>
      ".code *" #> price.code.toString &
      ".product-price *" #> price.price &
      ".product *" #> price.productName &
      ".delete [onclick]" #> SHtml.ajaxInvoke(deletePrice(price) _)
    }
  }
}
