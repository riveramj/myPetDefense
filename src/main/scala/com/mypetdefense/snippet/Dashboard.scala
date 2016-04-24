package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
import net.liftweb.mapper.{BySql, IHaveValidatedThisSQL}

import java.text.SimpleDateFormat
import java.util.Date

import com.mypetdefense.model._
import com.mypetdefense.actor._

object Dashboard extends Loggable with Dashboard {
  override val emailActor = EmailActor

  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Dashboard") / "dashboard"
}

trait Dashboard extends Loggable {
  def emailActor: EmailActor

  def getUpcomingShipments = {
    Subscription.findAll(
      BySql(
        "nextShipDate < CURRENT_DATE + interval '5 day' AND nextShipDate >= CURRENT_DATE", 
        IHaveValidatedThisSQL("mike","2016-04-24")
      )
    )
  }

  def shipProduct(parent: Box[Parent])() = {
    emailActor ! SendInvoicePaymentSucceededEmail(parent)
  }

  def render = {
    ".shipment" #> getUpcomingShipments.map { subscription =>
      val productNames = subscription.getProducts.groupBy(_.name)
      val dateFormat = new SimpleDateFormat("MMM dd")

      ".ship-on *" #> dateFormat.format(subscription.nextShipDate.get) &
      ".name *" #> subscription.parent.obj.map(_.name) &
      ".products" #> productNames.map { case (name, product) =>
        ".amount *" #> product.size &
        ".product-name *" #> name
      } &
      ".ship" #> SHtml.onSubmitUnit(shipProduct(subscription.parent.obj))
    }
  }
}
