package com.mypetdefense.snippet
package admin

import java.text.NumberFormat

import com.mypetdefense.model._
import com.mypetdefense.service._
import net.liftweb.common._
import net.liftweb.http.LiftResponse
import net.liftweb.http.SHtml.ajaxInvoke
import net.liftweb.http.js.JsCmd
import net.liftweb.mapper.By
import net.liftweb.sitemap.Loc.EarlyResponse
import net.liftweb.util.Helpers._
import net.liftweb.util._

case class UpdateChartData(
    chartName: String,
    newData: Array[Int],
    newLabels: Array[String] = Array()
) extends MyPetDefenseEvent("update-chart-data")

object ExecutiveDashboard extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Executive Dashboard") / "admin" / "executive-dashboard" >>
    mpdAdmin >>
    loggedIn

  val executiveSnapshotExportMenu: Menu.Menuable = Menu.i(
    "Export Executive Snapshot"
  ) / "admin" / "executive-dashboard" / "executive-snapshot.csv" >>
    mpdAdmin >>
    loggedIn >>
    EarlyResponse(exportexecutiveSnapshot _)

  def exportexecutiveSnapshot: Box[LiftResponse] = ReportingService.quickHitReport
}

class ExecutiveDashboard extends Loggable {
  val mpdAgency: Box[Agency] = Agency.find(By(Agency.name, "My Pet Defense"))
  val tppAgency: Box[Agency] = Agency.find(By(Agency.name, "TPP"))

  val topLevelAgencies = List(mpdAgency, tppAgency)

  val dollarFormatter: NumberFormat = NumberFormat.getCurrencyInstance
  val numberFormatter: NumberFormat = NumberFormat.getIntegerInstance

  val mtdShipments: List[Shipment] = Shipment.findMtdShipments
  val mtdShipmentValue: Double = mtdShipments.flatMap { shipment =>
    tryo(shipment.amountPaid.get.toDouble)
  }.foldLeft(0d)(_ + _)

  val todayShipments: List[Shipment] = Shipment.findTodayShipments
  val todayShipmentsValue: Double = todayShipments.flatMap { shipment =>
    tryo(shipment.amountPaid.get.toDouble)
  }.foldLeft(0d)(_ + _)

  val remainingMonthSubscriptions: List[Subscription] =
    Subscription.findCurrentMonthUpcomingSubscriptions

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

  val newStartsToday: List[Subscription] = Subscription.findNewTodaySubscriptions
  val newStartsTodayLastMonth: List[Subscription] =
    Subscription.findNewTodaySubscriptionsLastMonth
  val newStartsTodayLastYear: List[Subscription] =
    Subscription.findNewTodaySubscriptionsLastYear
  val newStartsTodayMonthDiff: Int = newStartsToday.size - newStartsTodayLastMonth.size
  val newStartsTodayYearDiff: Int  = newStartsToday.size - newStartsTodayLastYear.size

  val newStartsMTD: List[Subscription]          = Subscription.findNewMTDSubscriptions
  val newStartsMTDLastMonth: List[Subscription] = Subscription.findNewMTDSubscriptionsLastMonth
  val newStartsMTDLastYear: List[Subscription]  = Subscription.findNewMTDSubscriptionsLastYear
  val newStartsMTDMonthDiff: Int                = newStartsMTD.size - newStartsMTDLastMonth.size
  val newStartsMTDYearDiff: Int                 = newStartsMTD.size - newStartsMTDLastYear.size

  val newStartsYTD: List[Subscription]          = Subscription.findNewYTDSubscriptions
  val newStartsYTDLastMonth: List[Subscription] = Subscription.findNewYTDSubscriptionsLastMonth
  val newStartsYTDLastYear: List[Subscription]  = Subscription.findNewYTDSubscriptionsLastYear
  val newStartsYTDMonthDiff: Int                = newStartsYTD.size - newStartsYTDLastMonth.size
  val newStartsYTDYearDiff: Int                 = newStartsYTD.size - newStartsYTDLastYear.size

  val totalStarts: Int = newStartsToday.size + newStartsMTD.size + newStartsYTD.size

  def calcPercentage(numerator: Int, denominator: Int): Int =
    ((numerator / denominator.toDouble) * 100).round.toInt

  def calcStartPercentage(starts: Int): Int = calcPercentage(starts, totalStarts)

  def calcTodayPercentage(starts: Int): Int = calcPercentage(starts, newStartsToday.size)

  def calcMonthPercentage(starts: Int): Int = calcPercentage(starts, newStartsMTD.size)

  def calcYearPercentage(starts: Int): Int = calcPercentage(starts, newStartsYTD.size)

  def updateCharts(): JsCmd =
    UpdateChartData(
      "newStarts",
      Array(
        calcStartPercentage(newStartsYTD.size),
        calcStartPercentage(newStartsMTD.size),
        calcStartPercentage(newStartsToday.size)
      )
    )

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
    ".executive-snapshot-export [href]" #> ExecutiveDashboard.executiveSnapshotExportMenu.loc.calcDefaultHref &
    newStartBindings &
    ".executive-dashboard [class+]" #> "current" &
    ".update-data [onclick]" #> ajaxInvoke(() => updateCharts()) &
    ".mtd-shipments .count *" #> numberFormatter.format(mtdShipments.size) &
    ".mtd-shipments .value *" #> dollarFormatter.format(mtdShipmentValue) &
    ".today-shipments .count *" #> numberFormatter.format(todayShipments.size) &
    ".today-shipments .value *" #> dollarFormatter.format(todayShipmentsValue) &
    ".remaining-shipments-month .count *" #> numberFormatter.format(
      remainingMonthSubscriptions.size
    ) &
    ".remaining-shipments-month .value *" #> dollarFormatter.format(0.99) &
    ".mtd-users .new-users-count *" #> Subscription.findNewMTDSubscriptions.size &
    ".mtd-users .cancellations-count *" #> Subscription.findCancelledMtdSubscriptions.size
  }
}
