package com.mypetdefense.snippet
package admin

import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}

import com.mypetdefense.AppConstants.{DefaultLocale, DefaultTimezone}
import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.service._
import com.mypetdefense.util._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._
import net.liftweb.util._

import scala.xml.NodeSeq

object ShipmentDashboard extends Loggable {
  import net.liftweb.sitemap._
  import Loc._
  import com.mypetdefense.util.Paths._

  val menu: Menu.Menuable = Menu.i("Shipment Dashboard") / "admin" / "shipment-dashboard" >>
    mpdAdmin >>
    loggedIn

  val newLabelsExportMenu: Menu.Menuable =
    Menu.i("Export New Labels") / "admin" / "shipment-dashboard" / "export_new_shipments.csv" >>
      mpdAdmin >>
      loggedIn >>
      EarlyResponse(exportNewUspsLabels _)

  val existingLabelsExportMenu: Menu.Menuable =
    Menu.i("Export Existing Labels") / "admin" / "shipment-dashboard" / "export_existing_shipments.csv" >>
      mpdAdmin >>
      loggedIn >>
      EarlyResponse(exportExistingUspsLabels _)

  val mpdShipstationExportMenu: Menu.Menuable =
    Menu.i("Export MPD Shipstation Labels") / "admin" / "shipment-dashboard" / "export_MPD_shipstation.csv" >>
      mpdAdmin >>
      loggedIn >>
      EarlyResponse(exportMpdShipstationLabels _)

  def exportNewUspsLabels: Box[LiftResponse] = {
    LabelExportService.exportNewUspsLabels()
  }

  def exportExistingUspsLabels: Box[LiftResponse] = {
    LabelExportService.exportExistingUspsLabels()
  }

  def exportMpdShipstationLabels: Box[LiftResponse] = {
    LabelExportService.exportMpdShipStationLabels()
  }
}

class ShipmentDashboard extends Loggable {
  var shipmentRenderer: Box[IdMemoizeTransform] = Empty
  var future                                    = false

  val dateFormat                         = new SimpleDateFormat("MM/dd/yyyy")
  val localDateFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("MM/dd/yyyy", DefaultLocale)

  val currentDate: LocalDate = LocalDate.now()

  var fromDate: String = currentDate.format(localDateFormat)
  var toDate: String   = currentDate.plusDays(3).format(localDateFormat)

  def paidShipments: List[Shipment]           = ShipmentService.getCurrentPastDueShipments
  def futureSubscriptions: List[Subscription] = ShipmentService.getUpcomingSubscriptions

  def changeDataSet(dataSet: String): JsCmd = {
    dataSet match {
      case "current" =>
        future = false
      case "future" =>
        future = true
      case _ =>
    }

    shipmentRenderer.map(_.setHtml).openOr(Noop)
  }

  def paymentProcessed_?(shipment: Shipment): Boolean = {
    val paymentId = shipment.stripePaymentId.get
    !paymentId.isEmpty
  }

  def shipProduct(
      subscription: Box[Subscription],
      user: Box[User],
      shipment: Shipment,
      address: String,
      renderer: IdMemoizeTransform
  )(): JsCmd = {
    subscription.map { subscription => ParentService.updateNextShipDate(subscription) }

    shipment.dateShipped(ZonedDateTime.now(DefaultTimezone)).address(address).saveMe

    InventoryService.deductShipmentItems(shipment)

    EmailActor ! SendInvoicePaymentSucceededEmail(
      user,
      subscription,
      shipment.taxPaid.get,
      shipment.amountPaid.get,
      shipment.trackingNumber.get
    )

    renderer.setHtml
  }

  def shipmentHasShipped_?(shipment: Shipment): Boolean = {
    !tryo(shipment.dateShipped.get.toString).isEmpty
  }

  def fileUpload: NodeSeq => NodeSeq = {
    var fileHolder: Box[FileParamHolder] = Empty

    def uploadFile(file: FileParamHolder): JsCmd = {
      logger.info(
        "Received: %s [size=%d, type=%s]" format (file.fileName, file.length, file.mimeType)
      )
      val parsedFile    = TrackingUploadCSV.parse(file.file)
      val trackingInfos = parsedFile.map(_.list).openOr(Nil)

      trackingInfos.map(updateShipment)

      S.redirectTo(ShipmentDashboard.menu.loc.calcDefaultHref)
    }

    def updateShipment(trackingInfo: TrackingInfo) = {
      val shipments         = ShipmentService.getCurrentPastDueShipments
      val trackingRecipient = trackingInfo.recipient
      val name              = trackingRecipient.split(",").map(_.trim).headOption.getOrElse("")

      val matchingShipment: Option[Shipment] = shipments.find { shipment =>
        val shipmentUserName = {
          for {
            subscription <- shipment.subscription.obj
            user         <- subscription.user.obj
          } yield {
            user.name.trim
          }
        }.getOrElse("")

        name.toLowerCase == shipmentUserName.toLowerCase
      }

      matchingShipment.map { shipment =>
        shipment.trackingNumber(trackingInfo.trackingNumber).saveMe
      }
    }

    SHtml.makeFormsAjax andThen
      "#tracking-upload" #> SHtml.fileUpload { fph => fileHolder = Full(fph) } andThen
      "#upload-tracking-numbers" #> SHtml.ajaxOnSubmit(() => {

        fileHolder.map(uploadFile) openOr {
          logger.error("Got unexpected Empty when handling partner file upload.")
          S.error("Missing file")
        }
      })
  }

  def updateTrackingNumber(trackingNumber: String, shipment: Shipment): JsCmd = {
    if (!trackingNumber.trim.isEmpty) {
      val refreshedShipment = shipment.reload

      refreshedShipment.trackingNumber(trackingNumber).saveMe
    }

    Noop
  }

  def convertForecastingDates(date: String): LocalDate = {
    val parsedDate = dateFormat.parse(date)
    parsedDate.toInstant.atZone(DefaultTimezone).toLocalDate
  }

  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
      ".shipment-dashboard [class+]" #> "current" &
        ".new-export [href]" #> ShipmentDashboard.newLabelsExportMenu.loc.calcDefaultHref &
        ".existing-export [href]" #> ShipmentDashboard.existingLabelsExportMenu.loc.calcDefaultHref &
        ".shipstation-export [href]" #> ShipmentDashboard.mpdShipstationExportMenu.loc.calcDefaultHref &
        "#dashboard-current [onclick]" #> SHtml.ajaxInvoke(() => changeDataSet("current")) &
        "#dashboard-future [onclick]" #> SHtml.ajaxInvoke(() => changeDataSet("future")) &
        ".dashboard-details" #> SHtml.idMemoize { renderer =>
          shipmentRenderer = Full(renderer)

          def currentShipmentBindings = {
            ".forecast-dates" #> ClearNodes &
              ".shipment" #> paidShipments
                .sortBy(_.expectedShipDate.get.toInstant.toEpochMilli)
                .map { shipment =>
                  val subscription = shipment.subscription.obj
                  val user         = subscription.flatMap(_.user.obj)
                  val agencyName = {
                    for {
                      parent <- user
                      agency <- parent.referer
                    } yield {
                      agency.name.get
                    }
                  }.openOr("")

                  var trackingNumber = shipment.trackingNumber.get

                  val allShipments = subscription.map(_.shipments.toList).openOr(Nil)

                  val petsAndProducts = subscription.map(_.getPetAndProducts).openOr(Nil)
                  val dateFormat      = new SimpleDateFormat("MMM dd")

                  val shipAddressRaw = Address
                    .find(By(Address.user, user), By(Address.addressType, AddressType.Shipping))

                  val nameAddress = shipAddressRaw.map { ship =>
                    s"""${user.map(_.name).getOrElse("")}
            |${ship.street1}
            |${ship.street2}
            |${ship.city}, ${ship.state} ${ship.zip}""".stripMargin.replaceAll("\n\n", "\n")
                  }

                  ".name-address *" #> nameAddress &
                    ".product" #> petsAndProducts.map {
                      case (pet, product) =>
                        ".pet-name *" #> pet.map(_.name.get) &
                          ".product-name *" #> product.map(_.name.get) &
                          ".product-size *" #> product.map(_.size.get.toString)
                    } &
                    ".insert *" #> shipment.insert.get &
                    ".tracking" #> SHtml.ajaxText(
                      trackingNumber,
                      possibleTracking => updateTrackingNumber(possibleTracking, shipment)
                    ) &
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
                            shipButtonRenderer
                          ) _
                        )
                      }
                    }
                }
          }

          def futureShipmentBindings = {
            val startDate = convertForecastingDates(fromDate)
            val endDate   = convertForecastingDates(toDate)

            val selectedFutureSubscriptions = futureSubscriptions.filter { subscription =>
              val nextShipDate = subscription.getNextShipDate
              (nextShipDate.isAfter(startDate.minusDays(1)) && nextShipDate.isBefore(
                endDate.plusDays(1)
              ))
            }

            ".from-date" #> SHtml.ajaxText(fromDate, possibleFromDate => {
              fromDate = possibleFromDate
              renderer.setHtml
            }) &
              ".to-date" #> SHtml.ajaxText(toDate, possibleToDate => {
                toDate = possibleToDate
                renderer.setHtml
              }) &
              ".tracking-header" #> ClearNodes &
              ".ship-header" #> ClearNodes &
              ".shipment" #> selectedFutureSubscriptions
                .sortBy(_.nextShipDate.get.toInstant.toEpochMilli)
                .map { subscription =>
                  val user = subscription.user.obj

                  val petsAndProducts = subscription.getPetAndProducts
                  val dateFormat      = new SimpleDateFormat("MMM dd")

                  val shipAddressRaw = Address
                    .find(By(Address.user, user), By(Address.addressType, AddressType.Shipping))

                  val nameAddress = shipAddressRaw.map { ship =>
                    s"""${user.map(_.name).getOrElse("")}
            |${ship.street1}
            |${ship.street2}
            |${ship.city}, ${ship.state} ${ship.zip}""".stripMargin.replaceAll("\n\n", "\n")
                  }

                  ".name-address *" #> nameAddress &
                    ".product" #> petsAndProducts.map {
                      case (pet, product) =>
                        ".pet-name *" #> pet.map(_.name.get) &
                          ".product-name *" #> product.map(_.name.get) &
                          ".product-size *" #> product.map(_.size.get.toString)
                    } &
                    ".tracking-number" #> ClearNodes &
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
