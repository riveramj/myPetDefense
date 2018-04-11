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
import com.mypetdefense.service.{ParentService, ShipmentService}

object Dashboard extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Dashboard") / "admin" / "dashboard" >>
    adminUser >>
    loggedIn

  val exportMenu = Menu.i("Export CSV") / "admin" / "dashboard" / "export_data.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(exportCSV _)

  val shipmentsCSVMenu = Menu.i("Export Shipments") / "admin" / "dashboard" / "export_shipments.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(exportShipments _)

  def exportCSV: Box[LiftResponse] = {
    val csvHeaders = "Name" :: "Email" :: "Address" :: "Shipped Date" :: "Amount Paid" :: "Items Shipped" :: Nil
    
    val csvRows: List[List[String]] = {
      val parents = User.findAll(By(User.userType, UserType.Parent))
      val shipments = Shipment.findAll()
      val dateFormat = new SimpleDateFormat("MMM dd")
      
      {
        for {
          shipment <- shipments
          subscription <- shipment.subscription.obj
          user <- subscription.user.obj
          address <- user.addresses.toList.headOption
        } yield {
          val itemsList: List[ShipmentLineItem] = ShipmentLineItem.findAll(By(ShipmentLineItem.shipment, shipment))

          val itemsShipped: String = itemsList.flatMap(_.product.obj.map(_.getNameAndSize)).mkString("; ")

          user.name ::
          user.email.get ::
          s"${address.street1.get} ${address.street2.get} ${address.city.get} ${address.state.get} ${address.zip.get}" ::
          dateFormat.format(shipment.dateProcessed.get).toString ::
          shipment.amountPaid.toString ::
          itemsShipped.replaceAll(",","") :: 
          Nil
        }
      }
    }

    val resultingCsv = (List(csvHeaders) ++ csvRows).map(_.mkString(",")).mkString("\n")

      Some(new InMemoryResponse(
        resultingCsv.getBytes("UTF-8"),
        List(
          "Content-Type" -> "binary/octet-stream",
          "Content-Disposition" -> "attachment; filename=\"data.csv\""
          ),
        Nil,
        200
      ))
  }

  def exportShipments: Box[LiftResponse] = {
    val csvHeaders = "Order ID (required)" ::
                     "Order Date" ::
                     "Order Value" ::
                     "Requested Service" ::
                     "Ship To - Name" ::
                     "Ship To - Company" ::
                     "Ship To - Address 1" ::
                     "Ship To - Address 2" ::
                     "Ship To - Address 3" ::
                     "Ship To - State/Province" ::
                     "Ship To - City" ::
                     "Ship To - Postal Code" ::
                     "Ship To - Country" ::
                     "Ship To - Phone" ::
                     "Ship To - Email" ::
                     "Total Weight in Oz" ::
                     "Dimensions - Length" ::
                     "Dimensions - Width" ::
                     "Dimensions - Height" ::
                     "Notes - From Customer" ::
                     "Notes - Internal" ::
                     "Gift Wrap?" ::
                     "Gift Message" ::
                     Nil

    val csvRows: List[List[String]] = {
      val dateFormat = new SimpleDateFormat("MM/dd/yyyy")

      val subscriptions = ShipmentService.getPendingPastDueShipments

      {
        for {
          subscription <- subscriptions
          shipment <- Shipment.find(
                        By(Shipment.subscription, subscription),
                        By(Shipment.expectedShipDate, subscription.nextShipDate.get)
                      )
          user <- subscription.user.obj
          address <- Address.find(By(Address.user, user), By(Address.addressType, AddressType.Shipping))
        } yield {
          shipment.shipmentId.get.toString ::
          dateFormat.format(new Date()) ::
          "" ::
          "standard shipping" ::
          user.name ::
          "" ::
          address.street1.get ::
          address.street2.get ::
          "" ::
          address.city.get ::
          address.state.get ::
          address.zip.get ::
          "" ::
          "" ::
          "" ::
          "4" ::
          "" ::
          "" ::
          "" ::
          "" ::
          "" ::
          "" ::
          "" ::
          Nil
        }
      }
    }

    val resultingCsv = (List(csvHeaders) ++ csvRows).map(_.mkString(",")).mkString("\n")

    Some(new InMemoryResponse(
      resultingCsv.getBytes("UTF-8"),
      List(
        "Content-Type" -> "binary/octet-stream",
        "Content-Disposition" -> "attachment; filename=\"shipments.csv\""
        ),
      Nil,
      200
    ))
  }
}

class Dashboard extends Loggable {
  var subscriptionSet: List[Subscription] = Nil
  var shipmentRenderer: Box[IdMemoizeTransform] = Empty

  def currentShipments = ShipmentService.getCurrentShipments

  def pendingShipments = ShipmentService.getPendingShipments

  def pastDueShipments = ShipmentService.getPastDueShipments

  def updateSubscriptionSet(subscriptions: List[Subscription]) = {
    subscriptions.filter { subscription =>
      (subscription.status == Status.Active || subscription.status == Status.BillingSuspended)
    }
  }

  def changeSubscriptionSet(subscriptions: List[Subscription]) = {
    subscriptionSet = updateSubscriptionSet(subscriptions)

    shipmentRenderer.map(_.setHtml).openOr(Noop)
  }

  subscriptionSet = updateSubscriptionSet(currentShipments)

  def paymentProcessed_?(shipment: Box[Shipment]) = {
    val paymentId = shipment.map(_.stripePaymentId.get).openOr("")
    !paymentId.isEmpty
  }

  def shipProduct(
    subscription: Subscription,
    user: Box[User],
    shipment: Box[Shipment],
    address: String,
    renderer: IdMemoizeTransform
  )() = {
    val nextMonthLocalDate = LocalDate.now().plusMonths(1).atStartOfDay(ZoneId.systemDefault()).toInstant()
    val nextMonthDate = Date.from(nextMonthLocalDate)

    ParentService.updateNextShipBillDate(subscription, user, nextMonthDate)

    shipment.map(_.dateShipped(new Date()).address(address).saveMe)

    EmailActor ! SendInvoicePaymentSucceededEmail(
      user,
      subscription,
      shipment.map(_.taxPaid.get).openOr(""),
      shipment.map(_.amountPaid.get).openOr("")
    )
    
    renderer.setHtml
  }

  def shipmentHasShipped_?(possibleShipment: Box[Shipment]) = {
    val shippedDate = possibleShipment.map { shipment =>
      !tryo(shipment.dateShipped.get.toString).isEmpty
    }

    shippedDate.openOr(false)
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
      val subscriptions = ShipmentService.getCurrentPendingPastDueShipments
      val shipments = subscriptions.map { subscription =>
        Shipment.find(
          By(Shipment.subscription, subscription),
          By(Shipment.expectedShipDate, subscription.nextShipDate.get)
        )
      }.flatten

      val trackingRecipient = trackingInfo.recipient
      val name = trackingRecipient.split(",").map(_.trim).headOption.getOrElse("")

      val matchingShipment: Option[Shipment] = shipments.find { shipment =>
        val shipmentUserName = {
          for {
            subscription <- shipment.subscription.obj
            user <- subscription.user.obj
          } yield {
            user.name
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

  def render = {
    SHtml.makeFormsAjax andThen
    ".dashboard [class+]" #> "current" &
    "#csv-export [href]" #> Dashboard.exportMenu.loc.calcDefaultHref &
    "#shipments-export [href]" #> Dashboard.shipmentsCSVMenu.loc.calcDefaultHref &
    "#dashboard-current [onclick]" #> SHtml.ajaxInvoke(() => changeSubscriptionSet(currentShipments)) &
    "#dashboard-pending [onclick]" #> SHtml.ajaxInvoke(() => changeSubscriptionSet(pendingShipments)) &
    "#dashboard-past-due [onclick]" #> SHtml.ajaxInvoke(() => changeSubscriptionSet(pastDueShipments)) &
    ".dashboard-details" #> SHtml.idMemoize { renderer =>
      shipmentRenderer = Full(renderer)

      ".shipment" #> subscriptionSet.sortBy(_.nextShipDate.get.getTime).map { subscription =>

        val allShipments = subscription.shipments.toList
        val user = subscription.user.obj
        val agencyName = {
          for {
            parent <- user
            agency <- parent.referer
          } yield {
            agency.name.get
          }}.openOr("")


        val shipment = Shipment.find(
          By(Shipment.subscription, subscription),
          By(Shipment.expectedShipDate, subscription.nextShipDate.get)
        )

        val paymentProcessed = paymentProcessed_?(shipment)

        val insertNeeded = {
          (paymentProcessed, allShipments.size, agencyName) match {
            case (false, 0, "TPP") => "TPP+Welcome Insert"
            case (false, 0, _) => "Welcome Insert"
            case (true, 1, "TPP") => "TPP+Welcome Insert"
            case (true, 1, _) => "Welcome Insert"
            case (_, _, _) => "-"
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

        ".ship-on *" #> dateFormat.format(subscription.nextShipDate.get) &
        ".name-address *" #> nameAddress &
        ".product" #> petsAndProducts.map { case (pet, product) =>
          ".pet-name *" #> pet.name.get &
          ".product-name *" #> product.map(_.name.get) &
          ".product-size *" #> product.map(_.size.get.toString)
        } &
        ".insert *" #> insertNeeded &
        ".tracking-number *" #> shipment.map(_.trackingNumber.get) &
        ".ship-it" #> SHtml.idMemoize { shipButtonRenderer =>
          val updatedShipment = shipment.flatMap { possibleShipment =>
            Shipment.find(By(Shipment.shipmentId, possibleShipment.shipmentId.get))
          }

          if (shipmentHasShipped_?(shipment)) {
            ".ship [class+]" #> "shipped" &
            ".ship *" #> "Shipped" &
            ".ship [disabled]" #> "disabled"
          } else if (shipment.isEmpty || !paymentProcessed) {
            ".ship [class+]" #> "cant-ship" &
            ".ship *" #> "Can't Ship Yet." &
            ".ship [disabled]" #> "disabled"
          } else 
            ".ship [onclick]" #> SHtml.ajaxInvoke(shipProduct(subscription, user, shipment, nameAddress.openOr(""), shipButtonRenderer) _)
        }
      }
    }
  }
}
