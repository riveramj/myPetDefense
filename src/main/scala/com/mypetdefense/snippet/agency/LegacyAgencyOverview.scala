package com.mypetdefense.snippet
package agency

import com.mypetdefense.model._
import com.mypetdefense.service.ReportingService
import com.mypetdefense.util.SecurityContext
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._

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
    exportTPPMontSalesMenu.currentValue flatMap { monthYearAgency =>
      val tokens = monthYearAgency.split(" ")
      val month = tokens(0)
      val year  = tokens(1)
      val agency = tokens(2)
      ReportingService.exportAgencyMonthSales(agency, month, year)
    }
  }

  val menu: Menu.Menuable =
    Menu.i("Legacy Agency Overview") / "agency" / "legacy-agency-overview" >>
      adminUser >>
      loggedIn

  val agencyMtdYtdExportMenu: Menu.ParamMenuable[String] = Menu.param[String](
    "Agency Export Month to Date Sales",
    "Agency Export Month to Date Sales",
    Full(_),
    string => string
  ) / "agency" / "agency-overview" / "mtd-sales.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(agencyMtdYtdExport _)

  val exportTPPMontSalesMenu: Menu.ParamMenuable[String] = Menu.param[String](
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
  val currentUser: Box[User] = SecurityContext.currentUser
  val agency: Box[Agency]    = currentUser.flatMap(_.agency.obj)
  val agencyName: String     = agency.map(_.name.get).openOr("")

  def render: CssSel = {
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
      ".september-2019 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref(
        "September 2019"
      ) &
      ".october-2019 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("October 2019") &
      ".november-2019 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref(
        "November 2019"
      ) &
      ".december-2019 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref(
        "December 2019"
      ) &
      ".january-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("January 2020") &
      ".february-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref(
        "February 2020"
      ) &
      ".march-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("March 2020") &
      ".april-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("April 2020") &
      ".may-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("May 2020") &
      ".june-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("June 2020") &
      ".july-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("July 2020") &
      ".august-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("August 2020") &
      ".september-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref(
        "September 2020"
      ) &
      ".october-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("October 2020") &
      ".november-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("November 2020") &
      ".december-2020 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("December 2020") &
      ".january-2021 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("January 2021") &
      ".february-2021 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("February 2021") &
      ".march-2021 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("March 2021") &
      ".april-2021 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("April 2021") &
      ".may-2021 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("May 2021") &
      ".june-2021 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("June 2021") &
      ".july-2021-tpp [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("July 2021 TPP") &
      ".july-2021-mpd [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("July 2021 MPD") &
      ".july-2021-all [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("July 2021 All") &
      ".august-2021 [href]" #> LegacyAgencyOverview.exportTPPMontSalesMenu.calcHref("August 2021") &
      ".month-to-date-export [href]" #> LegacyAgencyOverview.agencyMtdYtdExportMenu.calcHref(
        agencyName
      )
  }
}
