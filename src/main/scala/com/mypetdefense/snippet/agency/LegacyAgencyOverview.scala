package com.mypetdefense.snippet
package agency

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
  import js.JsCmds._
import net.liftweb.mapper.By

import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service.{CouponService, ReportingService}
import com.mypetdefense.util.{ClearNodesIf, SecurityContext}

object LegacyAgencyOverview extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  def agencyMtdYtdExport: Box[LiftResponse] = {
    agencyMtdYtdExportMenu.currentValue flatMap { name =>
      ReportingService.exportAgencyMtdYtdSales(name)
    } 
  }

  def agencyMonthExport: Box[LiftResponse] = {
    exportTPPMontSalesMenu.currentValue flatMap { monthYear =>
      val month = monthYear.split(" ").headOption.getOrElse("")
      val year = monthYear.split(" ").lastOption.getOrElse("")
      ReportingService.exportAgencyMonthSales("TPP", month, year)
    } 
  }

  val menu = Menu.i("Legacy Agency Overview") / "agency" / "legacy-agency-overview" >>
    adminUser >>
    loggedIn

  val agencyMtdYtdExportMenu = Menu.param[String](
      "Agency Export Month to Date Sales",
      "Agency Export Month to Date Sales",
      Full(_),
      string => string
    ) / "agency" / "agency-overview" / "mtd-sales.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(agencyMtdYtdExport _)

  val exportTPPMontSalesMenu = Menu.param[String](
      "Agency Export Month Sales",
      "Agency Export Month  Sales",
      Full(_),
      string => string
    ) / "agency" / "agency-overview" / "month-sales.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(agencyMonthExport _)
}

class LegacyAgencyOverview extends Loggable {
  val currentUser = SecurityContext.currentUser
  val agency = currentUser.flatMap(_.agency.obj)
  val agencyName = agency.map(_.name.get).openOr("")

  def render = {
    ".overview [class+]" #> "current" &
    ".april [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("April 2018") &
    ".may [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("May 2018") &
    ".june [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("June 2018") &
    ".july [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("July 2018") &
    ".august [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("August 2018") &
    ".september [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("September 2018") &
    ".october [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("October 2018") &
    ".november [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("November 2018") &
    ".december [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("December 2018") &
    ".january [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("January 2019") &
    ".february [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("February 2019") &
    ".march [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("March 2019") &
    ".april [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("April 2019") &
    ".may [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("May 2019") &
    ".june-2019 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("June 2019") &
    ".july-2019 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("July 2019") &
    ".august-2019 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("August 2019") &
    ".september-2019 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("September 2019") &
    ".october-2019 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("October 2019") &
    ".november-2019 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("November 2019") &
    ".december-2019 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("December 2019") &
    ".january-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("January 2020") &
    ".february-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("February 2020") &
    ".march-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("March 2020") &
    ".april-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("April 2020") &
    ".may-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("May 2020") &
    ".june-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("June 2020") &
    ".month-to-date-export [href]" #> LegacyAgencyOverview.agencyMtdYtdExportMenu.calcHref(agencyName)
  }
}

