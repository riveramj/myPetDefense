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

object Dashboard extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

    val menu = Menu.i("Dashboard") / "admin" / "dashboard" >> 
      adminUser >>
      loggedIn
}

class Dashboard extends Loggable {

  def getUpcomingShipments = {
    Subscription.findAll(
      BySql(
        "nextShipDate < CURRENT_DATE + interval '5 day'",
        IHaveValidatedThisSQL("mike","2016-09-04")
      )
    )
  }

  def paymentProcessed_?(shipment: Box[Shipment]) = {
    val paymentId = shipment.map(_.stripePaymentId.get).openOr("")
    if (paymentId.isEmpty)
      "No"
    else
      "Yes"
  }

  def updateNextShipDate(subscription: Subscription) = {
    val nextMonthLocalDate = LocalDate.now().plusMonths(1).atStartOfDay(ZoneId.systemDefault()).toInstant()
    val nextMonthDate = Date.from(nextMonthLocalDate)
    val nextShipDate = subscription.nextShipDate(nextMonthDate)
    println(nextShipDate)
    nextShipDate.save
  }

  def shipProduct(subscription: Subscription, user: Box[User], shipment: Box[Shipment])() = {
    updateNextShipDate(subscription)
    EmailActor ! SendInvoicePaymentSucceededEmail(
      user,
      subscription,
      shipment.map(_.taxPaid.get).openOr(""),
      shipment.map(_.amountPaid.get).openOr("")
    )
  }

  def render = {
    ".dashboard [class+]" #> "current" &
    ".shipment" #> getUpcomingShipments.map { subscription =>
      val shipment = Shipment.find(
        By(Shipment.subscription, subscription),
        By(Shipment.expectedShipDate, subscription.nextShipDate.get)
      )

      val user = subscription.user.obj

      val products = subscription.getProducts
      val dateFormat = new SimpleDateFormat("MMM dd")

      val shipAddressRaw = Address.find(By(Address.user, user), By(Address.addressType, AddressType.Shipping))

      val address = shipAddressRaw.map { ship =>
        s"${ship.street1} / ${ship.street2} / ${ship.city}, ${ship.state} ${ship.zip}"
      }

      ".ship-on *" #> dateFormat.format(subscription.nextShipDate.get) &
      ".name *" #> user.map(_.name) &
      ".address *" #> address &
      ".products" #> products.map { product =>
        ".product-name *" #> product.name.get
        ".product-size *" #> product.size.get.toString
      } &
      ".payment-processed *" #> paymentProcessed_?(shipment) &
      ".ship" #> SHtml.onSubmitUnit(shipProduct(subscription, user, shipment))
    }
  }
}
