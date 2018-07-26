package com.mypetdefense.snippet
package admin 

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmd._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._
import net.liftweb.common._
import net.liftweb.http._
  import js.JsCmds._
import net.liftweb.mapper.{BySql, IHaveValidatedThisSQL, By}

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.Paths._
import com.mypetdefense.util._
import com.mypetdefense.actor._
import com.mypetdefense.service._

object Dashboard extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Dashboard") / "admin" / "dashboard" >>
    adminUser >>
    loggedIn

  val newLabelsExportMenu = Menu.i("Export New Labels") / "admin" / "dashboard" / "export_new_shipments.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(exportNewUspsLabels _)

  val existingLabelsExportMenu = Menu.i("Export Existing Labels") / "admin" / "dashboard" / "export_existing_shipments.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(exportExistingUspsLabels _)

  def exportNewUspsLabels: Box[LiftResponse] = {
    LabelExportService.exportNewUspsLabels()
  }

  def exportExistingUspsLabels: Box[LiftResponse] = {
    LabelExportService.exportExistingUspsLabels()
  }
}

class Dashboard extends Loggable {
  var shipmentRenderer: Box[IdMemoizeTransform] = Empty
  var future = false

  def paidShipments = ShipmentService.getCurrentPastDueShipments
  def futureShipments = ShipmentService.getUpcomingShipments

  def changeDataSet(dataSet: String) = {
    dataSet match {
      case "current" =>
        future = false
      case "future" =>
        future = true
      case _ =>
    }
  
    shipmentRenderer.map(_.setHtml).openOr(Noop)
  }

  def paymentProcessed_?(shipment: Shipment) = {
    val paymentId = shipment.stripePaymentId.get
    !paymentId.isEmpty
  }

  def shipProduct(
    subscription: Box[Subscription],
    user: Box[User],
    shipment: Shipment,
    address: String,
    renderer: IdMemoizeTransform,
    nextMonthDate: Date
  )() = {
    subscription.map { subscription =>
      ParentService.updateNextShipBillDate(subscription, user, nextMonthDate)
    }

    shipment.dateShipped(new Date()).address(address).saveMe

    EmailActor ! SendInvoicePaymentSucceededEmail(
      user,
      subscription,
      shipment.taxPaid.get,
      shipment.amountPaid.get,
      shipment.trackingNumber.get
    )
    
    renderer.setHtml
  }

  def shipmentHasShipped_?(shipment: Shipment) = {
    !tryo(shipment.dateShipped.get.toString).isEmpty
  }

  def fileUpload = {
    var fileHolder: Box[FileParamHolder] = Empty
    
    def uploadFile(file: FileParamHolder): JsCmd = {
      logger.info("Received: %s [size=%d, type=%s]" format(file.fileName, file.length, file.mimeType))
      val parsedFile = TrackingUploadCSV.parse(file.file)
      val trackingInfos = parsedFile.map(_.list).openOr(Nil)

      trackingInfos.map(updateShipment)

      S.redirectTo(Dashboard.menu.loc.calcDefaultHref)
    }

    def updateShipment(trackingInfo: TrackingInfo) = {
      val shipments = ShipmentService.getCurrentPastDueShipments
      val trackingRecipient = trackingInfo.recipient
      val name = trackingRecipient.split(",").map(_.trim).headOption.getOrElse("")

      val matchingShipment: Option[Shipment] = shipments.find { shipment =>
        val shipmentUserName = {
          for {
            subscription <- shipment.subscription.obj
            user <- subscription.user.obj
          } yield {
            user.name.trim
          }
        }.getOrElse("")

        name == shipmentUserName
      }

      matchingShipment.map { shipment =>
        shipment.trackingNumber(trackingInfo.trackingNumber).saveMe
      }
    }

    SHtml.makeFormsAjax andThen
    "#tracking-upload" #> SHtml.fileUpload { fph => 
      fileHolder = Full(fph)
    } andThen
    "#upload-tracking-numbers" #> SHtml.ajaxOnSubmit(() => {

      fileHolder.map(uploadFile) openOr {
        logger.error("Got unexpected Empty when handling partner file upload.")
        S.error("Missing file")
      }
    })  
  }

  def updateTrackingNumber(trackingNumber: String, shipment: Shipment) = {
    if (!trackingNumber.trim.isEmpty) {
      val refreshedShipment = shipment.refresh
    
      refreshedShipment.map(_.trackingNumber(trackingNumber).saveMe)
    }

    Noop
  }

  def render = {
    val nextMonthLocalDate = LocalDate.now().plusMonths(1).atStartOfDay(ZoneId.of("America/New_York")).toInstant()

    val nextMonthDate = Date.from(nextMonthLocalDate)

    SHtml.makeFormsAjax andThen
    ".dashboard [class+]" #> "current" &
    ".new-export [href]" #> Dashboard.newLabelsExportMenu.loc.calcDefaultHref &
    ".existing-export [href]" #> Dashboard.existingLabelsExportMenu.loc.calcDefaultHref &
    "#dashboard-current [onclick]" #> SHtml.ajaxInvoke(() => changeDataSet("current")) &
    "#dashboard-future [onclick]" #> SHtml.ajaxInvoke(() => changeDataSet("future")) &
    ".dashboard-details" #> SHtml.idMemoize { renderer =>
      shipmentRenderer = Full(renderer)
      
      def currentShipmentBindings = {
        ".shipment" #> paidShipments.sortBy(_.insert.get).sortBy(_.expectedShipDate.get.getTime).map { shipment =>

          val subscription = shipment.subscription.obj
          val user = subscription.flatMap(_.user.obj)
          val agencyName = {
            for {
              parent <- user
              agency <- parent.referer
              } yield {
                agency.name.get
          }}.openOr("")


          var trackingNumber = shipment.trackingNumber.get

          val allShipments = subscription.map(_.shipments.toList).openOr(Nil)

          val petsAndProducts = subscription.map(_.getPetAndProducts).openOr(Nil)
          val dateFormat = new SimpleDateFormat("MMM dd")

          val shipAddressRaw = Address.find(By(Address.user, user), By(Address.addressType, AddressType.Shipping))

          val nameAddress = shipAddressRaw.map { ship =>
          s"""${user.map(_.name).getOrElse("")}
            |${ship.street1}
            |${ship.street2}
            |${ship.city}, ${ship.state} ${ship.zip}""".stripMargin.replaceAll("\n\n", "\n")
          }

          ".name-address *" #> nameAddress &
          ".product" #> petsAndProducts.map { case (pet, product) =>
            ".pet-name *" #> pet.name.get &
            ".product-name *" #> product.map(_.name.get) &
            ".product-size *" #> product.map(_.size.get.toString)
          } &
          ".insert *" #> shipment.insert.get &
          ".tracking" #> SHtml.ajaxText(trackingNumber, possibleTracking => updateTrackingNumber(possibleTracking, shipment)) &
          ".ship-it" #> SHtml.idMemoize { shipButtonRenderer =>
            if (shipmentHasShipped_?(shipment)) {
              ".ship [class+]" #> "shipped" &
              ".ship *" #> "Shipped" &
              ".ship [disabled]" #> "disabled"
            } else {
              ".ship [onclick]" #> SHtml.ajaxInvoke(
                shipProduct(
                  subscription,
                  user,
                  shipment,
                  nameAddress.openOr(""),
                  shipButtonRenderer,
                  nextMonthDate
                )
              _)
            }
          }
        }
      }

      def futureShipmentBindings = {
      ".shipment" #> futureShipments.sortBy(_.nextShipDate.get.getTime).map { subscription =>

          val user = subscription.user.obj

          val agencyName = {
            for {
              parent <- user
              agency <- parent.referer
              } yield {
                agency.name.get
          }}.openOr("")


          val allShipments = subscription.shipments.toList

          val insertNeeded = {
           (allShipments.size, agencyName) match {
             case (0, "TPP") => "TPP+Welcome Insert"
             case (0, _) => "Welcome Insert"
             case (_, _) => "-"
           }
         }

          val petsAndProducts = subscription.getPetAndProducts
          val dateFormat = new SimpleDateFormat("MMM dd")

          val shipAddressRaw = Address.find(By(Address.user, user), By(Address.addressType, AddressType.Shipping))

          val nameAddress = shipAddressRaw.map { ship =>
          s"""${user.map(_.name).getOrElse("")}
            |${ship.street1}
            |${ship.street2}
            |${ship.city}, ${ship.state} ${ship.zip}""".stripMargin.replaceAll("\n\n", "\n")
          }

          ".name-address *" #> nameAddress &
          ".product" #> petsAndProducts.map { case (pet, product) =>
            ".pet-name *" #> pet.name.get &
            ".product-name *" #> product.map(_.name.get) &
            ".product-size *" #> product.map(_.size.get.toString)
          } &
          ".insert *" #> insertNeeded &
          ".tracking" #> ClearNodes &
          ".ship-it" #> ClearNodes
        }
      }

      if (future) {
        futureShipmentBindings
      } else {
        currentShipmentBindings
      }
    }
  }
}
