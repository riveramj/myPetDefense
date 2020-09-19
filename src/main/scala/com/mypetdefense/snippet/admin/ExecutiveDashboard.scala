package com.mypetdefense.snippet
package admin 

import java.text.NumberFormat

import com.mypetdefense.model._
import com.mypetdefense.service._
import net.liftweb.common._
import net.liftweb.http.SHtml.ajaxInvoke
import net.liftweb.http.js.JsCmd
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._
import net.liftweb.util._

case class UpdateChartData(chartName: String, newData: Array[Int], newLabels: Array[String] = Array()) extends MyPetDefenseEvent("update-chart-data")

object ExecutiveDashboard extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Executive Dashboard") / "admin" / "executive-dashboard" >>
    mpdAdmin >>
    loggedIn 
}

class ExecutiveDashboard extends Loggable {
  val mpdAgency: Box[Agency] = Agency.find(By(Agency.name, "My Pet Defense"))
  val tppAgency: Box[Agency] = Agency.find(By(Agency.name, "TPP"))

  val topLevelAgencies = List(mpdAgency, tppAgency)

  val dollarFormatter: NumberFormat = NumberFormat.getCurrencyInstance
  val numberFormatter: NumberFormat = NumberFormat.getIntegerInstance

  val mtdShipments: List[Shipment] = ReportingService.findMtdShipments
  val mtdShipmentValue: Double = mtdShipments.flatMap { shipment =>
    tryo(shipment.amountPaid.get.toDouble)
  }.foldLeft(0D)(_+_)

  val todayShipments: List[Shipment] = ReportingService.findTodayShipments
  val todayShipmentsValue: Double = todayShipments.flatMap { shipment =>
    tryo(shipment.amountPaid.get.toDouble)
  }.foldLeft(0D)(_+_)

  val remainingMonthSubscriptions: List[Subscription] = ReportingService.findCurrentMonthUpcomingSubscriptions

  /*
  val remainingMonthValue = {
    for {
      subscription <- remainingMonthSubscriptions
      product <- subscription.getProducts
      priceCode = subscription.priceCode.get
      price <- Price.getPricesByCode(product, priceCode)
    } yield {
      price.price.get
    }
  }.sum
   */

  val newStartsToday: List[Subscription] = ReportingService.findNewTodaySubscriptions
  val newStartsTodayLastMonth: List[Subscription] = ReportingService.findNewTodaySubscriptionsLastMonth
  val newStartsTodayLastYear: List[Subscription] = ReportingService.findNewTodaySubscriptionsLastYear
  val newStartsTodayMonthDiff: Int = newStartsToday.size - newStartsTodayLastMonth.size
  val newStartsTodayYearDiff: Int = newStartsToday.size - newStartsTodayLastYear.size


  val newStartsMTD: List[Subscription]  = ReportingService.findNewMTDSubscriptions
  val newStartsMTDLastMonth: List[Subscription] = ReportingService.findNewMTDSubscriptionsLastMonth
  val newStartsMTDLastYear: List[Subscription] = ReportingService.findNewMTDSubscriptionsLastYear
  val newStartsMTDMonthDiff: Int = newStartsMTD.size - newStartsMTDLastMonth.size
  val newStartsMTDYearDiff: Int = newStartsMTD.size - newStartsMTDLastYear.size

  val newStartsYTD: List[Subscription]  = ReportingService.findNewYTDSubscriptions
  val newStartsYTDLastMonth: List[Subscription] = ReportingService.findNewYTDSubscriptionsLastMonth
  val newStartsYTDLastYear: List[Subscription] = ReportingService.findNewYTDSubscriptionsLastYear
  val newStartsYTDMonthDiff: Int = newStartsMTD.size - newStartsYTDLastMonth.size
  val newStartsYTDYearDiff: Int = newStartsMTD.size - newStartsYTDLastYear.size

  val totalStarts: Int = newStartsToday.size + newStartsMTD.size + newStartsYTD.size

  def calcPercentage(numerator: Int, denominator: Int): Int = ((numerator/denominator.toDouble) * 100).round.toInt

  def calcStartPercentage(starts: Int): Int = calcPercentage(starts, totalStarts)

  def calcTodayPercentage(starts: Int): Int = calcPercentage(starts, newStartsToday.size)

  def calcMonthPercentage(starts: Int): Int = calcPercentage(starts, newStartsMTD.size)

  def calcYearPercentage(starts: Int): Int = calcPercentage(starts, newStartsYTD.size)

  def updateCharts(): JsCmd =
    UpdateChartData("newStarts", Array(calcStartPercentage(newStartsToday.size), calcStartPercentage(newStartsMTD.size), calcStartPercentage(newStartsYTD.size)))

  def newStartBindings: CssSel = {
    ".new-starts .key-stat .key-table" #> {
      ".today-stats" #> {
        ".new-starts *" #> newStartsToday.size &
        ".new-starts-last-month-diff *" #> (newStartsToday.size - newStartsTodayLastMonth.size) &
        ".new-starts-last-month-percent *" #> calcTodayPercentage(newStartsTodayMonthDiff) &
        ".new-starts-last-year-diff *" #> (newStartsToday.size - newStartsTodayLastYear.size) &
        ".new-starts-last-year-percent *" #> calcTodayPercentage(newStartsTodayYearDiff)
      } &
      ".mtd-stats" #> {
        ".new-starts *" #> newStartsMTD.size &
        ".new-starts-last-month-diff *" #> (newStartsMTD.size - newStartsMTDLastMonth.size) &
        ".new-starts-last-month-percent *" #> calcMonthPercentage(newStartsMTDMonthDiff) &
        ".new-starts-last-year-diff *" #> (newStartsMTD.size - newStartsMTDLastYear.size) &
        ".new-starts-last-year-percent *" #> calcMonthPercentage(newStartsMTDYearDiff)
      } &
      ".ytd-stats" #> {
        ".new-starts *" #> newStartsYTD.size &
        ".new-starts-last-month-diff *" #> (newStartsYTD.size - newStartsYTDLastMonth.size) &
        ".new-starts-last-month-percent *" #> calcYearPercentage(newStartsYTDMonthDiff) &
        ".new-starts-last-year-diff *" #> (newStartsYTD.size - newStartsYTDLastYear.size) &
        ".new-starts-last-year-percent *" #> calcYearPercentage(newStartsYTDYearDiff)
      }
    }
  }

  def render: CssBindFunc = {
    newStartBindings &
    ".executive-dashboard [class+]" #> "current" &
    ".update-data [onclick]" #> ajaxInvoke(() => updateCharts()) &
    ".mtd-shipments .count *"#> numberFormatter.format(mtdShipments.size) &
    ".mtd-shipments .value *"#> dollarFormatter.format(mtdShipmentValue) &
    ".today-shipments .count *"#> numberFormatter.format(todayShipments.size) &
    ".today-shipments .value *"#> dollarFormatter.format(todayShipmentsValue) &
    ".remaining-shipments-month .count *"#> numberFormatter.format(remainingMonthSubscriptions.size) &
    ".remaining-shipments-month .value *"#> dollarFormatter.format(0.99) &
    ".mtd-users .new-users-count *"#> ReportingService.findNewMTDSubscriptions.size &
    ".mtd-users .cancellations-count *"#> ReportingService.findCancelledMtdSubscriptions.size
  }
}
