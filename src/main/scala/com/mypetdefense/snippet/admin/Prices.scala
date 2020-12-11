package com.mypetdefense.snippet
package admin

import java.time.LocalDate

import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._

import scala.xml.{Elem, NodeSeq}

object Prices extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Prices") / "admin" / "prices" >>
    mpdAdmin >>
    loggedIn
}

case class DisplayPrice(code: String, price: Double, productName: String)

class Prices extends Loggable {
  val productNames: List[String] = FleaTick.findAll().map(_.name.get).distinct
  val allPrices: List[Price]     = Price.findAll()
  val prices: List[DisplayPrice] = allPrices.map { price =>
    DisplayPrice(price.code.get, price.price.get, price.fleaTick.obj.map(_.name.get).openOr(""))
  }.distinct

  var code          = ""
  var rawPrice      = ""
  var chosenProduct = ""

  def productDropdown: Elem = {
    SHtml.select(
      ("", "") +: productNames.sortWith(_ < _).map(name => (name, name)),
      Full(chosenProduct),
      chosenProduct = _
    )
  }

  def createPrice: JsCmd = {
    val price = tryo(rawPrice.trim().toDouble).getOrElse(0d)
    val validateFields = List(
      checkEmpty(code, "#code"),
      checkEmpty(price.toString, "#price")
    ).flatten

    if (validateFields.isEmpty) {
      val priceDbId = generateLongId
      val date      = LocalDate.now
      val name      = s"${chosenProduct} (${code} ${date})"

      val selectedProducts = FleaTick.findAll(By(FleaTick.name, chosenProduct))
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

  def deletePrice(price: DisplayPrice)(): Nothing = {
    val products = FleaTick.findAll(By(FleaTick.name, price.productName))
    products.map { product =>
      val prices = Price.findAll(By(Price.fleaTick, product), By(Price.code, price.code))
      prices.map(_.delete_!)
    }

    S.redirectTo(Prices.menu.loc.calcDefaultHref)
  }

  def render: NodeSeq => NodeSeq = {
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
