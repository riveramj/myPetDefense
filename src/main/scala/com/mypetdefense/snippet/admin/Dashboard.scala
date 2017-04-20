package com.mypetdefense.snippet
package admin 

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
import net.liftweb.mapper.{BySql, IHaveValidatedThisSQL, By}

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.Paths._
import com.mypetdefense.actor._
import com.mypetdefense.service.ParentService

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
}

class Dashboard extends Loggable {

  def upcomingShipments = {
    Subscription.findAll(
      BySql(
        "nextShipDate < CURRENT_DATE + interval '5 day'",
        IHaveValidatedThisSQL("mike","2016-09-04")
      )
    )
  }

  val activeSubscriptions = upcomingShipments.filter { subscription =>
    val pets = subscription.user.obj.map(_.activePets).getOrElse(Nil)
    pets.length > 0
  }

  def paymentProcessed_?(shipment: Box[Shipment]) = {
    val paymentId = shipment.map(_.stripePaymentId.get).openOr("")
    if (paymentId.isEmpty)
      "No"
    else
      "Yes"
  }

  def shipProduct(subscription: Subscription, user: Box[User], shipment: Box[Shipment])() = {
    val nextMonthLocalDate = LocalDate.now().plusMonths(1).atStartOfDay(ZoneId.systemDefault()).toInstant()
    val nextMonthDate = Date.from(nextMonthLocalDate)

    ParentService.updateNextShipBillDate(subscription, user, nextMonthDate)

    EmailActor ! SendInvoicePaymentSucceededEmail(
      user,
      subscription,
      shipment.map(_.taxPaid.get).openOr(""),
      shipment.map(_.amountPaid.get).openOr("")
    )
  }

  def render = {
    ".dashboard [class+]" #> "current" &
    "#csv-export [href]" #> Dashboard.exportMenu.loc.calcDefaultHref &
    ".shipment" #> activeSubscriptions.map { subscription =>
      val shipment = Shipment.find(
        By(Shipment.subscription, subscription),
        By(Shipment.expectedShipDate, subscription.nextShipDate.get)
      )

      val user = subscription.user.obj

      val petsAndProducts = subscription.getPetAndProducts
      val dateFormat = new SimpleDateFormat("MMM dd")

      val shipAddressRaw = Address.find(By(Address.user, user), By(Address.addressType, AddressType.Shipping))

      val address = shipAddressRaw.map { ship =>
        s"""${ship.street1}
          |${ship.street2}
          |${ship.city}, ${ship.state} ${ship.zip}""".stripMargin.replaceAll("\n\n", "\n")
      }

      ".ship-on *" #> dateFormat.format(subscription.nextShipDate.get) &
      ".name *" #> user.map(_.nameAndEmail) &
      ".address *" #> address &
      ".product" #> petsAndProducts.map { case (pet, product) =>
        ".pet-name *" #> pet.name.get &
        ".product-name *" #> product.map(_.name.get) &
        ".product-size *" #> product.map(_.size.get.toString)
      } &
      ".payment-processed *" #> paymentProcessed_?(shipment) &
      ".ship" #> SHtml.onSubmitUnit(shipProduct(subscription, user, shipment))
    }
  }
}
