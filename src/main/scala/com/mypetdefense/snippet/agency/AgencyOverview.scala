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
}

class AgencyOverview extends Loggable {
  val currentUser = SecurityContext.currentUser
  val agency = currentUser.flatMap(_.agency.obj)
  val agencyName = agency.map(_.name.get).openOr("")

  def render = {
    ".overview [class+]" #> "current" &
    ".month-to-date-export [href]" #> AgencyOverview.agencyMtdYtdExportMenu.calcHref(agencyName)
  }
}

