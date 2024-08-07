package com.mypetdefense.snippet
package admin

import java.text.NumberFormat
import com.mypetdefense.model.Agency._
import com.mypetdefense.model.domain.reports._
import com.mypetdefense.service._
import com.mypetdefense.util.DateHelper
import net.liftweb.common._
import net.liftweb.http.{IdMemoizeTransform, LiftResponse, SHtml}
import net.liftweb.http.SHtml.{ajaxInvoke, ajaxText}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.Noop
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
    EarlyResponse(exportExecutiveSnapshot _)

  def exportExecutiveSnapshot: Box[LiftResponse] = ReportingService.executiveSnapshot

  val userUpgradeExportMenu: Menu.Menuable = Menu.i(
    "Export User Upgrade Report"
  ) / "admin" / "executive-dashboard" / "user-upgrade-report.csv" >>
    mpdAdmin >>
    loggedIn >>
    EarlyResponse(exportUserUpgradeReport _)

  val customerLifespanExportMenu: Menu.Menuable = Menu.i(
    "Customer Lifespan Upgrade Report"
  ) / "admin" / "executive-dashboard" / "customer-lifespan-report.csv" >>
    mpdAdmin >>
    loggedIn >>
    EarlyResponse(exportCustomerLifespanReport _)

  def exportUserUpgradeReport: Box[LiftResponse] = ReportingService.basicUsersUpgradeCsv

  def exportCustomerLifespanReport: Box[LiftResponse] = ReportingService.customerLifespanCsv

  val retentionSnapshotExportMenu: Menu.Menuable = Menu.i(
    "Export Retention Snapshot"
  ) / "admin" / "executive-dashboard" / "retention-snapshot.csv" >>
    mpdAdmin >>
    loggedIn >>
    EarlyResponse(retentionExecutiveSnapshot _)

  def retentionExecutiveSnapshot: Box[LiftResponse] = ReportingService.subscriptionRetentionCsv()

  val snapshotInTimeExportMenu: Menu.ParamMenuable[String] = Menu.param[String](
    "Export Snapshot in Time",
    "Export Snapshot in Time",
    Full(_),
    string => string
  ) / "admin" / "executive-dashboard" / "snapshot-in-time.csv" >>
    mpdAdmin >>
    loggedIn >>
    EarlyResponse(snapshotInTime _)

  def snapshotInTime: Box[LiftResponse] = snapshotInTimeExportMenu.currentValue.flatMap { date =>
    ReportingService.snapshotInTimeCsv(date)
  }
}

class ExecutiveDashboard extends Loggable {
  var snapshotDate: String = DateHelper.dateFormat.format(DateHelper.yesterdayStart)
  var snapshotInTimeRenderer: Box[IdMemoizeTransform] = Empty

  val topLevelAgencies = List(mpdAgency, tppAgency)

  val dollarFormatter: NumberFormat    = NumberFormat.getCurrencyInstance
  val numberFormatter: NumberFormat    = NumberFormat.getIntegerInstance
  val report: ExecutiveDashboardReport = ReportingService.executiveDashboardReport

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

  val todayData: TodayRelatedData            = report.newStartsTodayData
  val mtdData: MTDData                       = report.newStartsMTDData
  val ytdData: YTDData                       = report.newStartsYTDData
  val todayShipmentsData: TodayShipmentsData = report.todayShipmentsData
  val mtdShipmentsData: MTDShipmentsData     = report.mtdShipmentData

  def updateCharts(): JsCmd = UpdateChartData("newStarts", report.newStartsPercentage)

  def newStartBindings: CssSel = {
    ".new-starts .key-stat .key-table" #> {
      ".today-stats" #> {
        ".new-starts *" #> todayData.newStartsToday &
          ".new-starts-last-month-diff *" #> todayData.monthDiff &
          ".new-starts-last-month-percent *" #> todayData.monthDiffPercentage &
          ".new-starts-last-year-diff *" #> todayData.yearDiff &
          ".new-starts-last-year-percent *" #> todayData.yearDiffPercentage
      } &
        ".mtd-stats" #> {
          ".new-starts *" #> mtdData.newStartsMTD &
            ".new-starts-last-month-diff *" #> mtdData.monthDiff &
            ".new-starts-last-month-percent *" #> mtdData.monthDiffPercentage &
            ".new-starts-last-year-diff *" #> mtdData.yearDiff &
            ".new-starts-last-year-percent *" #> mtdData.yearDiffPercentage
        } &
        ".ytd-stats" #> {
          ".new-starts *" #> ytdData.newStartsYTD &
            ".new-starts-last-month-diff *" #> ytdData.monthDiff &
            ".new-starts-last-month-percent *" #> ytdData.monthDiffPercentage &
            ".new-starts-last-year-diff *" #> ytdData.yearDiff &
            ".new-starts-last-year-percent *" #> ytdData.yearDiffPercentage
        }
    }
  }

  def createExportLink = ExecutiveDashboard.snapshotInTimeExportMenu.calcHref(snapshotDate)

  def render: CssBindFunc = {
    newStartBindings &
      ".executive-dashboard [class+]" #> "current" &
      ".update-data [onclick]" #> ajaxInvoke(() => updateCharts()) &
      ".snapshot-in-time li #date-for-snapshot" #> ajaxText(snapshotDate, date => {
        snapshotDate = date
        snapshotInTimeRenderer.map(_.setHtml()).openOr(Noop)
      }) &
      ".snapshot-in-time .snapshot-in-time-container" #> SHtml.idMemoize { renderer =>
        snapshotInTimeRenderer = Full(renderer)
        ".snapshot-in-time-export [href]" #> createExportLink
      } &
      ".executive-snapshot .executive-snapshot-export [href]" #> ExecutiveDashboard.executiveSnapshotExportMenu.loc.calcDefaultHref &
      ".upgrade-stats .user-upgrade-export [href]" #> ExecutiveDashboard.userUpgradeExportMenu.loc.calcDefaultHref &
      ".customer-lifespan-stats .customer-lifespan-export [href]" #> ExecutiveDashboard.customerLifespanExportMenu.loc.calcDefaultHref &
      ".retention-snapshot .retention-snapshot-export [href]" #> ExecutiveDashboard.retentionSnapshotExportMenu.loc.calcDefaultHref &
      ".mtd-shipments .count *" #> numberFormatter.format(mtdShipmentsData.numberOfShipments) &
      ".mtd-shipments .value *" #> dollarFormatter.format(mtdShipmentsData.totalAmount.toDouble) &
      ".today-shipments .count *" #> numberFormatter.format(todayShipmentsData.numberOfShipments) &
      ".today-shipments .value *" #> dollarFormatter.format(todayShipmentsData.totalAmount.toDouble) &
      ".remaining-shipments-month .count *" #> numberFormatter.format(
        report.remainingMonthSubscriptions
      ) &
      ".remaining-shipments-month .value *" #> dollarFormatter.format(0.99) &
      ".mtd-users .new-users-count *" #> report.newUserCount &
      ".mtd-users .cancellations-count *" #> report.cancellationsCount
  }
}
