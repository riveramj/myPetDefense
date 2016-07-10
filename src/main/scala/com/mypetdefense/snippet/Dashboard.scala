package com.mypetdefense.snippet

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
import com.mypetdefense.actor._

object Dashboard extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Dashboard") / "dashboard"
}

class Dashboard extends Loggable {

  def getUpcomingShipments = {
    Subscription.findAll(
      BySql(
        "nextShipDate < CURRENT_DATE + interval '5 day' AND nextShipDate >= CURRENT_DATE", 
        IHaveValidatedThisSQL("mike","2016-04-24")
      )
    )
  }

  def paymentProcessed_?(subscription: Subscription) = {
    val shipment = Shipment.find(
      By(Shipment.subscription, subscription),
      By(Shipment.expectedShipDate, subscription.nextShipDate.get)
    )

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

  def shipProduct(subscription: Subscription, user: Box[User])() = {
    updateNextShipDate(subscription)
    EmailActor ! SendInvoicePaymentSucceededEmail(user)
  }

  def render = {
    ".shipment" #> getUpcomingShipments.map { subscription =>
      val productNames = subscription.getProducts.groupBy(_.name)
      val dateFormat = new SimpleDateFormat("MMM dd")

      ".ship-on *" #> dateFormat.format(subscription.nextShipDate.get) &
      ".name *" #> subscription.user.obj.map(_.name) &
      ".products" #> productNames.map { case (name, product) =>
        ".amount *"  #> product.size &
        ".product-name *" #> name
      } &
      ".payment-processed *" #> paymentProcessed_?(subscription) &
      ".ship" #> SHtml.onSubmitUnit(shipProduct(subscription, subscription.user.obj))
    }
  }
}
