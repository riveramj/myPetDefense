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
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.actor._

object Prices extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Prices") / "admin" / "prices" >>
    adminUser >>
    loggedIn
}

class Prices extends Loggable {
  val productLines = Product.findAll()
  val prices = Price.findAll()

  var code = ""
  var rawPrice = ""
  var chosenProduct: Box[Product] = Empty
  
  def productDropdown = {
    SHtml.selectObj(
        productLines.map(product => (product, product.name.get)),
        chosenProduct,
        (product: Product) => chosenProduct = Full(product)
      )
  }

  def createPrice = {
    val price = tryo(rawPrice.trim().toDouble).getOrElse(0D)
    val validateFields = List(
      checkEmpty(code, "#code"),
      checkEmpty(price.toString, "#price")
    ).flatten

    if(validateFields.isEmpty) {
      val newPrice = Price.createPrice(price, code, chosenProduct)
      
      S.redirectTo(Prices.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
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
    ".price-entry" #> prices.map { price =>
      ".code *" #> price.code.toString &
      ".product-price *" #> price.price &
      ".product *" #> price.product.obj.map(_.name.get) &
      ".status *" #> price.active
    }
  }
}



