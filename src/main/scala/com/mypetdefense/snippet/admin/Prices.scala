package com.mypetdefense.snippet
package admin

import com.mypetdefense.model.{BoxType, _}
import com.mypetdefense.service.StripeFacade
import com.mypetdefense.service.ValidationService._
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers._

import java.time.LocalDate
import scala.xml.{Elem, NodeSeq}

object Prices extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Prices") / "admin" / "prices" >>
    mpdAdmin >>
    loggedIn
}

class Prices extends Loggable {
  val fleaTick: List[FleaTick] = FleaTick.findAll()
  val allPrices: List[Price]     = Price.findAll()
  val boxTypes: List[BoxType.Value] = BoxType.values.toList

  var code          = ""
  var rawPrice      = ""
  var chosenProduct: Box[FleaTick] = Empty
  var chosenBoxType: Box[BoxType.Value] = Empty

  def productDropdown: Elem = {
    SHtml.selectObj(
      List((Empty, "Choose Product")) ++ fleaTick.map(fleaTick => (Full(fleaTick), fleaTick.getNameAndSize)),
      Full(chosenProduct),
      (ft: Box[FleaTick]) => chosenProduct = ft
    )
  }

  def boxTypeDropdown: Elem = {
    SHtml.selectObj(
      List((Empty, "Box Type")) ++ boxTypes.map(boxType => (Full(boxType), boxType.toString)),
      Full(chosenBoxType),
      (possibleBoxType: Box[BoxType.Value]) =>
        chosenBoxType = possibleBoxType
    )
  }

  def createPrice: JsCmd = {
    val validateFields = List(
      checkEmpty(code, "#code"),
      checkNumber(rawPrice, "#price"),
      checkEmpty(chosenBoxType, "#box-type-select"),
      checkEmpty(chosenProduct, "#product-select"),
    ).flatten

    if (validateFields.isEmpty) {
      (for {
        fleaTick <- chosenProduct
        date       = LocalDate.now
        codeName   = s"$code $date"
        price <- tryo(rawPrice.trim().toDouble)
      } yield {
        val newPrice = Price.createPrice(price, codeName, fleaTick, "", boxType = chosenBoxType)
        val cost = (price * 100).toLong

        val stripePrice = StripeFacade.Price.create(newPrice.stripeProductId.get, cost, code)
        stripePrice.map(p => newPrice.stripePriceId(p.id).saveMe())

        S.redirectTo(Prices.menu.loc.calcDefaultHref)
      }).openOrThrowException("Failed to create price")
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def deletePrice(price: Price)(): Nothing = {
    price.delete_!

    S.redirectTo(Prices.menu.loc.calcDefaultHref)
  }

  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
    ".prices [class+]" #> "current" &
    ".create" #> idMemoize { renderer =>
      "#code" #> ajaxText(code, code = _) &
      "#price" #> ajaxText(rawPrice, rawPrice = _) &
      "#product-container #product-select" #> productDropdown &
      "#box-type-container #box-type-select" #> boxTypeDropdown &
      "#create-item" #> SHtml.ajaxSubmit("Create Price", () => createPrice)
    } &
    ".price-entry" #> allPrices.sortWith(_.code.get < _.code.get).map { price =>
      ".code *" #> price.code.get &
      ".product-price *" #> price.price &
      ".product *" #> price.fleaTick.map(_.getNameAndSize) &
      ".box-type *" #> price.boxType.get.toString &
      ".delete [onclick]" #> SHtml.ajaxInvoke(deletePrice(price) _)
    }
  }
}
