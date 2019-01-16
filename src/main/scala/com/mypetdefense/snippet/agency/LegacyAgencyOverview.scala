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
    exportTPPMontSalesMenu.currentValue flatMap { month =>
      ReportingService.exportAgencyMonthSales("TPP", month)
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
    ".april [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("April") &
    ".may [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("May") &
    ".june [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("June") &
    ".july [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("July") &
    ".august [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("August") &
    ".september [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("September") &
    ".october [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("October") &
    ".november [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("November") &
    ".december [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("December") &
    ".month-to-date-export [href]" #> LegacyAgencyOverview.agencyMtdYtdExportMenu.calcHref(agencyName)
  }
}

