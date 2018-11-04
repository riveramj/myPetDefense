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

object AgencyOverview extends Loggable {
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

  val menu = Menu.i("Agency Overview") / "agency" / "agency-overview" >>
    agencyUser >>
    loggedIn

  val agencyMtdYtdExportMenu = Menu.param[String](
      "Agency Export Month to Date Sales",
      "Agency Export Month to Date Sales",
      Full(_),
      string => string
    ) / "agency" / "agency-overview" / "mtd-sales.csv" >>
    agencyUser >>
    loggedIn >>
    EarlyResponse(agencyMtdYtdExport _)

  val exportTPPMontSalesMenu = Menu.param[String](
      "Agency Export Month Sales",
      "Agency Export Month  Sales",
      Full(_),
      string => string
    ) / "agency" / "agency-overview" / "month-sales.csv" >>
    agencyUser >>
    loggedIn >>
    EarlyResponse(agencyMonthExport _)
}

class AgencyOverview extends Loggable {
  val currentUser = SecurityContext.currentUser
  val agency = currentUser.flatMap(_.agency.obj)
  val agencyName = agency.map(_.name.get).openOr("")

  def render = {
    ".overview [class+]" #> "current" &
    ".april [href]" #> AgencyOverview.exportTPPMontSalesMenu.calcHref("April") &
    ".may [href]" #> AgencyOverview.exportTPPMontSalesMenu.calcHref("May") &
    ".june [href]" #> AgencyOverview.exportTPPMontSalesMenu.calcHref("June") &
    ".july [href]" #> AgencyOverview.exportTPPMontSalesMenu.calcHref("July") &
    ".august [href]" #> AgencyOverview.exportTPPMontSalesMenu.calcHref("August") &
    ".september [href]" #> AgencyOverview.exportTPPMontSalesMenu.calcHref("September") &
    ".october [href]" #> AgencyOverview.exportTPPMontSalesMenu.calcHref("October") &
    ".november [href]" #> AgencyOverview.exportTPPMontSalesMenu.calcHref("November") &
    ".month-to-date-export [href]" #> AgencyOverview.agencyMtdYtdExportMenu.calcHref(agencyName)
  }
}

