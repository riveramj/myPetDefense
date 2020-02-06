package com.mypetdefense.snippet
package admin

import net.liftweb._
  import sitemap.Menu
  import http.SHtml._
  import http._
  import js.JsCmds._

import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmd._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util._
import com.mypetdefense.service._
  import ValidationService._
import com.mypetdefense.actor._

import com.mypetdefense.util.RandomIdGenerator._

object AmazonOrders extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Amazon Orders") / "admin" / "amazon-orders" >>
    mpdAdmin >>
    loggedIn
}

class AmazonOrders extends Loggable {
  var newOrders: List[AmazonOrder] = Nil
  var newOrdersRenderer: Box[IdMemoizeTransform] = Empty

  val dateFormat = new SimpleDateFormat("MMM dd, yyyy")

  def fileUpload = {
    var fileHolder: Box[FileParamHolder] = Empty

    def uploadFile(file: FileParamHolder): JsCmd = {
      logger.info("Received: %s [size=%d, type=%s]" format(file.fileName, file.length, file.mimeType))
      val parsedFile = AmazonOrderUploadCsv.parse(file.file)
      newOrders = parsedFile.map(_.list).openOr(Nil)
      newOrdersRenderer.map(_.setHtml).openOr(Noop)
    }

    def createReviews() = {
      newOrders.map(_.orderId(generateLongId).saveMe)

      S.redirectTo(AmazonOrders.menu.loc.calcDefaultHref)
    }

    SHtml.makeFormsAjax andThen
    "#review-upload" #> SHtml.fileUpload {fph =>
      fileHolder = Full(fph)
    } andThen
    "#upload-reviews" #> SHtml.ajaxOnSubmit(() => {
      fileHolder.map(uploadFile) openOr {
        logger.error("Got unexpected Empty when handling partner file upload.")
        S.error("Missing file")
      }
    }) &
    "#create-reviews" #> SHtml.ajaxOnSubmit(createReviews _) &
    ".new-reviews" #> SHtml.idMemoize { renderer =>
      newOrdersRenderer = Full(renderer)

      ".new-review" #> newOrders.map { order =>
        ".amazon-id *" #> order.amazonOrderId.get &
        ".email *" #> order.email.get &
        ".phone *" #> order.phone.get &
        ".sku *" #> order.sku.get &
        ".quantity *" #> order.quantityPurchased.get &
        ".price *" #> order.productPrice.get &
        ".discount *" #> order.productDiscount.get &
        ".carrier *" #> order.carrier.get &
        ".product-name *" #> order.productName.get &
        ".name *" #> order.name.get &
        ".address-1 *" #> order.address1.get &
        ".address-2 *" #> order.address2.get &
        ".address-3 *" #> order.address3.get &
        ".city *" #> order.city.get &
        ".state *" #> order.state.get &
        ".zip *" #> order.zip.get &
        ".purchase-date *" #> dateFormat.format(order.purchaseDate.get)
      }
    }
  }

  def render = {
    val allOrders = AmazonOrder.findAll()

    SHtml.makeFormsAjax andThen
    ".amazon-orders [class+]" #> "current" &
    ".review" #> allOrders.map { order =>
      ".amazon-id *" #> order.amazonOrderId.get &
      ".email *" #> order.email.get &
      ".phone *" #> order.phone.get &
      ".sku *" #> order.sku.get &
      ".quantity *" #> order.quantityPurchased.get &
      ".price *" #> order.productPrice.get &
      ".discount *" #> order.productDiscount.get &
      ".carrier *" #> order.carrier.get &
      ".product-name *" #> order.productName.get &
      ".name *" #> order.name.get &
      ".address-1 *" #> order.address1.get &
      ".address-2 *" #> order.address2.get &
      ".address-3 *" #> order.address3.get &
      ".city *" #> order.city.get &
      ".state *" #> order.state.get &
      ".zip *" #> order.zip.get &
      ".purchase-date *" #> dateFormat.format(order.purchaseDate.get)
    }
  }
}
