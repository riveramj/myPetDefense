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

  def exportMonthToDateSalesResponse: Box[LiftResponse] = {
    agencyMonthToDateExportMenu.currentValue flatMap { name =>
      ReportingService.exportMonthToDateSales(name)
    } 
  }

  def exportCancellationDataResponse: Box[LiftResponse] = {
    agencyCancellationExportMenu.currentValue flatMap { name =>
      ReportingService.exportCancellationData(name)
    } 
  }

  val menu = Menu.i("Agency Overview") / "agency" / "agency-overview" >>
    agencyUser >>
    loggedIn

  val agencyMonthToDateExportMenu = Menu.param[String](
      "Agency Export Month to Date Sales",
      "Agency Export Month to Date Sales",
      Full(_),
      string => string
    ) / "agency" / "agency-overview" / "mtd-sales.csv" >>
    agencyUser >>
    loggedIn >>
    EarlyResponse(exportMonthToDateSalesResponse _)

  val agencyCancellationExportMenu = Menu.param[String](
      "Agency Export Cancellation Data",
      "Agency Export Cancellation Data",
      Full(_),
      string => string
    ) / "agency" / "agency-overview" / "cancellation-data.csv" >>
    agencyUser >>
    loggedIn >>
    EarlyResponse(exportCancellationDataResponse _)
}

class AgencyOverview extends Loggable {
  val currentUser = SecurityContext.currentUser
  val agency = currentUser.flatMap(_.agency.obj)
  val agencyName = agency.map(_.name.get).openOr("")

  def render = {
    ".overview [class+]" #> "current" &
    ".cancellation-export [href]" #> AgencyOverview.agencyCancellationExportMenu.calcHref(agencyName) &
    ".month-to-date-sales-export [href]" #> AgencyOverview.agencyMonthToDateExportMenu.calcHref(agencyName)
  }
}

