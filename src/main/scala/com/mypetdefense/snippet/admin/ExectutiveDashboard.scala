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
import net.liftweb.mapper.{BySql, IHaveValidatedThisSQL, By, NotBy}

import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import java.time.{LocalDate, ZoneId}
import java.time.format.DateTimeFormatter

import com.mypetdefense.model._
import com.mypetdefense.util.Paths._
import com.mypetdefense.util._
import com.mypetdefense.actor._
import com.mypetdefense.service._

object ExecutiveDashboard extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Executive Dashboard") / "admin" / "executive-dashboard" >>
    mpdAdmin >>
    loggedIn 
}

class ExecutiveDashboard extends Loggable {
  val mpdAgency = Agency.find(By(Agency.name, "My Pet Defense"))
  val tppAgency = Agency.find(By(Agency.name, "TPP"))

  val topLevelAgencies = List(mpdAgency, tppAgency)

  val mtdShipments = ReportingService.findMtdShipments
  val mtdShipmentValue = mtdShipments.map { shipment =>
    tryo(shipment.amountPaid.get.toDouble)
  }.flatten.foldLeft(0D)(_+_)

  val todayShipments = ReportingService.findTodayShipments
  val todayShipmentsValue = todayShipments.map { shipment =>
    tryo(shipment.amountPaid.get.toDouble)
  }.flatten.foldLeft(0D)(_+_)

  val currentMonthUpcomingSubscriptions = ReportingService.findCurrentMonthUpcomingSubscriptions

  val currentMonthUpcomingValue = {
    println("in for loop")
    for {
      subscription <- currentMonthUpcomingSubscriptions
      product <- subscription.getProducts
      _ = println(product)
      priceCode = subscription.priceCode.get
      _ = println(priceCode)
      price <- Price.getPricesByCode(product, priceCode)
      _ = println(price)
    } yield {
      _ = println(price.price.get)
      price.price.get
    }
  }.sum

  val totalMonthShipments = mtdShipments.size + currentMonthUpcomingSubscriptions.size

  val totalMonthShipmentValue = todayShipmentsValue + currentMonthUpcomingValue

  def render = {
    ".executive-dashboard [class+]" #> "current" &
    ".mtd-shipments .count *"#> mtdShipments.size &
    ".mtd-shipments .value *"#> f"$$$mtdShipmentValue%2.2f" &
    ".today-shipments .count *"#> todayShipments.size &
    ".today-shipments .value *"#> f"$$$todayShipmentsValue%2.2f" &
    ".total-month-shipments .count *"#> totalMonthShipments &
    ".total-month-shipments .value *"#> f"$$$totalMonthShipmentValue%2.2f" &
    ".mtd-users .new-users-count *"#> ReportingService.findNewMtdSubscriptions.size &
    ".mtd-users .cancellations-count *"#> ReportingService.findCancelledMtdSubscriptions.size &
    ".agency" #> topLevelAgencies.map { agency =>
      val customers: List[User] = { 
        if (agency.map(_.name.get == "My Pet Defense").openOr(false)) {
          agency.map(_.customers.toList).openOr(Nil)
        } else {
          Agency.findAll(NotBy(Agency.name, "My Pet Defense")).map(_.customers.toList).flatten
        }
      }

      val subscriptions = customers.flatMap(_.getSubscription)
      val subscriptionsByStatus = subscriptions.groupBy(_.status.get)
      val activeSubscriptions = tryo(subscriptionsByStatus(Status.Active)).openOr(Nil)
      val cancelledSubscriptions = tryo(subscriptionsByStatus(Status.Cancelled)).openOr(Nil)
      val subscriptionMinusActive = tryo(subscriptionsByStatus - Status.Active).openOr(subscriptionsByStatus)
      val suspendedSubscriptionsRaw = tryo(subscriptionMinusActive - Status.Cancelled).openOr(subscriptionMinusActive)
      val suspendedSubscriptions = suspendedSubscriptionsRaw.values
      
      ".name *" #> agency.map(_.name.get) &
      ".active-count *" #> activeSubscriptions.size &
      ".suspended-count *" #> cancelledSubscriptions.size &
      ".cancelled-count *" #> suspendedSubscriptions.size
    }
  }
}
