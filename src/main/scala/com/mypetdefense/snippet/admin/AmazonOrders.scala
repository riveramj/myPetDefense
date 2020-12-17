package com.mypetdefense.snippet
package admin

import java.text.SimpleDateFormat

import com.mypetdefense.model._
import com.mypetdefense.service._
import com.mypetdefense.util.RandomIdGenerator._
import com.mypetdefense.util.csv.AmazonOrderUploadCsv
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

case class AmazonOrderExport(
    startDate: Box[String],
    endDate: Box[String],
    animalType: Box[AnimalType.Value]
)

object AmazonOrders extends Loggable {
  import net.liftweb.sitemap._
  import Loc._
  import com.mypetdefense.util.Paths._

  def amazonOrderExport: Box[LiftResponse] = {
    exportAmazonOrder.currentValue flatMap { export => ReportingService.exportAmazonOrders(export) }
  }

  def getExportParameters(rawQuery: String): AmazonOrderExport = {
    println(rawQuery + " 1234")
    val query     = rawQuery.split(",")
    val startDate = query.find(_.startsWith("startDate")).map(_.substring(10))
    val endDate   = query.find(_.startsWith("endDate")).map(_.substring(8))
    val petType   = query.find(_.startsWith("animalType")).map(_.substring(11))
    println(petType + " 111111")

    val animalType =
      if (petType.exists(_.toLowerCase.trim == "dog"))
        Full(AnimalType.Dog)
      else if (petType.exists(_.toLowerCase.trim == "cat"))
        Full(AnimalType.Cat)
      else
        Empty

    println(startDate)
    println(endDate)
    println(animalType)

    AmazonOrderExport(startDate, endDate, animalType)
  }

  def createExportParameters(order: AmazonOrderExport): String = {
    s"startDate=${order.startDate.getOrElse("")},endDate=${order.endDate.getOrElse("")},animalType=${order.animalType
      .getOrElse("")}"
  }

  val menu: Menu.Menuable = Menu.i("Amazon Orders") / "admin" / "amazon-orders" >>
    mpdAdmin >>
    loggedIn

  val exportAmazonOrder: Menu.ParamMenuable[AmazonOrderExport] = Menu.param[AmazonOrderExport](
    "Export Orders",
    "Export Orders",
    exportParameters => Full(getExportParameters(exportParameters)),
    export => createExportParameters(export)
  ) / "admin" / "amazon-orders" / "export-orders" >>
    mpdAdmin >>
    loggedIn >>
    EarlyResponse(amazonOrderExport _)
}

class AmazonOrders extends Loggable {
  var newOrders: List[AmazonOrder]               = Nil
  var newOrdersRenderer: Box[IdMemoizeTransform] = Empty
  var startDate                                  = ""
  var endDate                                    = ""
  var petType                                    = ""

  val dateFormat = new SimpleDateFormat("MMM dd, yyyy")

  def fileUpload: NodeSeq => NodeSeq = {
    var fileHolder: Box[FileParamHolder] = Empty

    def uploadFile(file: FileParamHolder): JsCmd = {
      logger.info(
        "Received: %s [size=%d, type=%s]" format (file.fileName, file.length, file.mimeType)
      )
      val parsedFile = AmazonOrderUploadCsv.parse(file.file)
      newOrders = parsedFile.openOr(Nil)
      newOrdersRenderer.map(_.setHtml).openOr(Noop)
    }

    def createReviews() = {
      newOrders.map(_.orderId(generateLongId).saveMe)

      S.redirectTo(AmazonOrders.menu.loc.calcDefaultHref)
    }

    SHtml.makeFormsAjax andThen
      "#review-upload" #> SHtml.fileUpload { fph => fileHolder = Full(fph) } andThen
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

  def generateExportQuery: AmazonOrderExport = {
    val animalType =
      if (petType.toLowerCase.trim == "dog")
        AnimalType.Dog
      else
        AnimalType.Cat

    AmazonOrderExport(Some(startDate), Some(endDate), Some(animalType))
  }

  def render: NodeSeq => NodeSeq = {
    val allOrders = AmazonOrder.findAll()

    SHtml.makeFormsAjax andThen
      ".amazon-orders [class+]" #> "current" &
        "#export-results" #> {
          "#start-date" #> SHtml.text(startDate, startDate = _) &
            "#end-date" #> SHtml.text(endDate, endDate = _) &
            "#pet-type" #> SHtml.text(petType, petType = _) &
            "#download" #> SHtml.ajaxSubmit(
              "Download",
              () => JsCmds.RedirectTo(AmazonOrders.exportAmazonOrder.calcHref(generateExportQuery))
            )
        } &
        ".review" #> allOrders.map { order =>
          val address = (s"""${order.address1.get}
      ${order.address2.get}
    ${order.address3.get}
      ${order.city.get}, ${order.state.get} ${order.zip.get}
      """).replaceAll("  ", "").replaceAll("\n\n", "").trim()
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
            ".address *" #> address &
            ".purchase-date *" #> dateFormat.format(order.purchaseDate.get)
        }
  }
}
