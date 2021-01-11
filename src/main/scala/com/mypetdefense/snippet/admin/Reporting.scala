package com.mypetdefense.snippet
package admin

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.mypetdefense.AppConstants.{DefaultLocale, DefaultTimezone}
import com.mypetdefense.model._
import com.mypetdefense.service.ReportingService
import com.mypetdefense.util.DateHelper.dateFormat
import com.mypetdefense.util.ModelSyntax._
import com.mypetdefense.util.ProductNameHelper
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._

import scala.collection.immutable.ListMap

object Reporting extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Reporting") / "admin" / "reporting" >>
    mpdAdmin >>
    loggedIn
}

class Reporting extends Loggable {
  val agencies: List[String]               = Agency.findAll().map(_.name.get)
  val allSubscriptions: List[Subscription] = Subscription.upgradedActiveSubscriptions

  val localDateFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("MM/dd/yyyy", DefaultLocale)
  val currentDate: LocalDate             = LocalDate.now()

  var fromDate: String = currentDate.plusDays(14).format(localDateFormat)
  var toDate: String   = currentDate.plusDays(14).format(localDateFormat)

  val forecastingCounts: CssSel = {
    def upcomingSubscriptionProducts: List[String] = {
      val startDate = convertForecastingDates(fromDate)
      val endDate   = convertForecastingDates(toDate)

      val upcomingSubscriptions = allSubscriptions.filter { subscription =>
        val nextShipDate = subscription.getNextShipDate

        nextShipDate.isAfter(startDate.minusDays(1)) && nextShipDate.isBefore(endDate.plusDays(1))
      }

      val fleaTicks =
        for {
          sub      <- upcomingSubscriptions
          box      <- sub.subscriptionBoxes
          fleaTick <- box.fleaTick
        } yield fleaTick.getNameAndSize

      val products =
        for {
          sub     <- upcomingSubscriptions
          box     <- sub.subscriptionBoxes
          item    <- box.subscriptionItems
          product <- item.product
        } yield product.name.get

      fleaTicks ++ products
    }

    ".forecasting" #> idMemoize { renderer =>
      val sanitizedNames = upcomingSubscriptionProducts.map {
        case product if product.contains("5-22") =>
          "ZoGuard Plus for Dogs 05-22 lbs"
        case product if product.contains("3-10") =>
          "Adventure Plus for Dogs, 3-10 lbs"
        case product if product.contains("5-15") =>
          "ShieldTec Plus for Dogs, 05-15 lbs"
        case product =>
          product
      }

      val upcomingCounts = sanitizedNames.groupBy(identity).mapValues(_.size).toList

      val sanitizedNamesSorted = ListMap(upcomingCounts.sortBy(_._1): _*)

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
        val users              = Agency.getUsersForAgency(agencyName)
        val subscriptions      = users.getSubscriptions
        val cancelsByShipment  = ReportingService.cancelsByShipment(subscriptions)
        val shipments          = ReportingService.getShipments(subscriptions)
        val averageShipments   = shipments.size.toDouble / subscriptions.size.toDouble
        val totalCancellations = cancelsByShipment.map(_._2).sum

        ".agency-name *" #> agencyName &
          ".shipments *" #> {
            if (agencyName == "TPP")
              f"$averageShipments%.1f"
            else
              f"$averageShipments%.1f*"
          } &
          ".cancel-detail" #> cancelsByShipment.sorted.map {
            case (shipmentCount, cancellations) =>
              val cancellationRate = cancellations / totalCancellations.toDouble

              ".shipment-count *" #> shipmentCount &
                ".cancellations *" #> cancellations &
                ".cancel-rate *" #> convertToPercentage(cancellationRate)
          }
      } &
      forecastingCounts
  }

  private def convertToPercentage(percent: Double) = f"${percent * 100}%.1f%%"

  private def convertForecastingDates(date: String): LocalDate =
    dateFormat.parse(date).toInstant.atZone(DefaultTimezone).toLocalDate

  private def upcomingSubscriptionProducts: List[String] = {
    val startDate = convertForecastingDates(fromDate)
    val endDate   = convertForecastingDates(toDate)

    val upcomingSubscriptions = allSubscriptions.filter { subscription =>
      val nextShipDate = subscription.getNextShipDate

      nextShipDate.isAfter(startDate.minusDays(1)) && nextShipDate.isBefore(endDate.plusDays(1))
    }

    upcomingSubscriptions
      .flatMap(_.subscriptionBoxes.toList.flatMap(_.fleaTick.obj))
      .map(_.getNameAndSize)
  }

  private[snippet] def getSanitizedSortedNames: ListMap[String, Int] = {
    val sanitizedNames = ProductNameHelper.sanitizeFleaTickNames(upcomingSubscriptionProducts)
    val upcomingCounts = sanitizedNames.groupBy(identity).mapValues(_.size).toList
    ListMap(upcomingCounts.sortBy(_._1): _*)
  }
}
