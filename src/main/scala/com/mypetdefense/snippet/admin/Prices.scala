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

class Prices extends Loggable {
  val productNames = Product.findAll().map(_.name.get).distinct
  val prices = Price.findAll()

  var code = ""
  var rawPrice = ""
  var chosenProduct = ""
  
  def productDropdown = {
    SHtml.select(
      ("","") +: productNames.map(name => (name,name)),
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

      val stripePrice = PriceService.createStripePrice(
        price,
        priceDbId,
        name
      )

      stripePrice match {
        case Full(_) =>
          for {
            product <- selectedProducts
          } yield {
            Price.createPrice(priceDbId, price, code, product, name)
          }

          S.redirectTo(Prices.menu.loc.calcDefaultHref)

        case _ =>
          Noop
      }
      
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def deletePrice(price: Price)() = {
    price.delete_!
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
    ".price-entry" #> prices.map { price =>
      ".code *" #> price.code.toString &
      ".product-price *" #> price.price &
      ".product *" #> price.product.obj.map(_.name.get) &
      ".status *" #> price.active &
      ".delete [onclick]" #> SHtml.ajaxInvoke(deletePrice(price))
    }
  }
}



