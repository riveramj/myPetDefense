package com.mypetdefense.snippet
package admin

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mapper._

import scala.collection.immutable.ListMap
import com.mypetdefense.model._
import com.mypetdefense.service.ReportingService
import com.mypetdefense.util.ModelSyntax._
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import java.time.{LocalDate, ZoneId}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import net.liftweb.util.CssSel

object Reporting extends Loggable {
  import net.liftweb.sitemap._
  import Loc._
  import com.mypetdefense.util.Paths._

  val menu: Menu.Menuable = Menu.i("Reporting") / "admin" / "reporting" >>
    mpdAdmin >>
    loggedIn
}

class Reporting extends Loggable {
  val agencies: List[String] = Agency.findAll().map(_.name.get)
  val allSubscriptions: List[Subscription] = Subscription.findAll(
    By(Subscription.status, Status.Active)
  ) //TODO: make this use the dynamic dates

  val dateFormat                         = new SimpleDateFormat("MM/dd/yyyy")
  val localDateFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("MM/dd/yyyy", Locale.ENGLISH)
  val currentDate: LocalDate             = LocalDate.now()

  var fromDate: String = currentDate.plusDays(14).format(localDateFormat)
  var toDate: String   = currentDate.plusDays(14).format(localDateFormat)

  def convertToPercentage(percent: Double) = f"${percent * 100}%.1f%%"

  def convertForecastingDates(date: String): LocalDate = {

    val parsedDate = dateFormat.parse(date)

    parsedDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()
  }

  val forecastingCounts: CssSel = {
    def upcomingSubscriptionProducts = {
      val startDate = convertForecastingDates(fromDate)
      val endDate   = convertForecastingDates(toDate)

      val upcomingSubscriptions = allSubscriptions.filter { subscription =>
        val nextShipDate = subscription.getNextShipDate

        (nextShipDate.isAfter(startDate.minusDays(1)) && nextShipDate.isBefore(endDate.plusDays(1)))
      }

      upcomingSubscriptions
        .flatMap(_.subscriptionBoxes.toList.flatMap(_.fleaTick.obj))
        .map(_.getNameAndSize)
    }

    ".forecasting" #> idMemoize { renderer =>
      val sanitizedNames = upcomingSubscriptionProducts.map { name =>
        name match {
          case product if product.contains("5-22") =>
            "ZoGuard Plus for Dogs 05-22 lbs"
          case product if product.contains("3-10") =>
            "Adventure Plus for Dogs, 3-10 lbs"
          case product if product.contains("5-15") =>
            "ShieldTec Plus for Dogs, 05-15 lbs"
          case product =>
            product
        }
      }

      val upcomingCounts = sanitizedNames.groupBy(identity).mapValues(_.size).toList

      val sanitizedNamesSorted = ListMap(upcomingCounts.toSeq.sortBy(_._1): _*)

      ".from-date" #> SHtml.ajaxText(fromDate, possibleFromDate => {
        fromDate = possibleFromDate
        renderer.setHtml
      }) &
        ".to-date" #> SHtml.ajaxText(toDate, possibleToDate => {
          toDate = possibleToDate
          renderer.setHtml
        }) &
        ".product-info " #> sanitizedNamesSorted.map {
          case (productName, count) =>
            ".product *" #> productName &
              ".count *" #> count
        }
    }
  }

  def render: CssSel = {
    ".reporting [class+]" #> "current" &
      ".agency" #> agencies.map { agencyName =>
        val users              = ReportingService.getUsersForAgency(agencyName)
        val subscriptions      = users.getSubscriptions
        val cancelsByShipment  = ReportingService.cancelsByShipment(subscriptions)
        val shipments          = ReportingService.getShipments(subscriptions)
        val averageShipments   = shipments.size.toDouble / subscriptions.size.toDouble
        val totalCancellations = cancelsByShipment.map(_._2).foldLeft(0)(_ + _)

        ".agency-name *" #> agencyName &
          ".shipments *" #> {
            if (agencyName == "TPP")
              f"$averageShipments%.1f"
            else
              f"$averageShipments%.1f*"
          } &
          ".cancel-detail" #> cancelsByShipment.toSeq.sorted.map {
            case (shipmentCount, cancellations) =>
              val cancellationRate = cancellations / totalCancellations.toDouble

              ".shipment-count *" #> shipmentCount &
                ".cancellations *" #> cancellations &
                ".cancel-rate *" #> convertToPercentage(cancellationRate)
          }
      } &
      forecastingCounts
  }
}
