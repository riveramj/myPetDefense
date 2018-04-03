package com.mypetdefense.snippet
package admin

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
import com.mypetdefense.util.ClearNodesIf

import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import java.time.{LocalDate, ZoneId, LocalDateTime}
import java.time.format.DateTimeFormatter

object Agencies extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Agencies") / "admin" / "agencies" >>
    adminUser >>
    loggedIn

  val totalSalesExportMenu = Menu.i("Export Total Sales") / "admin" / "agencies" / "month-year-gross-sales.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(ReportingService.exportTotalSales _)

  val salesDataExportMenu = Menu.i("Export Gross Sales") / "admin" / "agencies" / "raw-sales.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(ReportingService.exportRawSales _)

  val monthToDateExportMenu = Menu.i("Export Month to Date Sales") / "admin" / "agencies" / "mtd-sales.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(ReportingService.exportMonthToDateSales _)

  val cancellationExportMenu = Menu.i("Export Cancellation Data") / "admin" / "agencies" / "cancellation-data.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(ReportingService.exportCancellationData _)
}

class Agencies extends Loggable {
  var name = ""

  val agencies = Agency.findAll()

  def createAgency = {
    val validateFields = List(
      checkEmpty(name, "#name")
    ).flatten

    if(validateFields.isEmpty) {
      Agency.createNewAgency(
        name.trim()
      )
      S.redirectTo(Agencies.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def deleteAgency(agency: Agency)() = {

    val possibleAgency = Agency.find(By(Agency.agencyId, agency.agencyId.get))

    for {
      refreshedAgency <- possibleAgency.toList
      member <- refreshedAgency.members
    } yield member.delete_!

    for {
      refreshedAgency <- possibleAgency.toList
      coupon <- refreshedAgency.coupons
    } yield CouponService.deleteCoupon(coupon)
    
    if (agency.delete_!)
      S.redirectTo(Agencies.menu.loc.calcDefaultHref)
    else
      Alert("An error has occured. Please try again.")
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".agencies [class+]" #> "current" &
    "#name" #> text(name, name = _) &
    "#create-item" #> SHtml.ajaxSubmit("Create Agency", () => createAgency) &
    ".agency" #> agencies.map { agency =>
      ".name *" #> agency.name &
      ".customer-count *" #> agency.customers.size &
      ".coupon-count *" #> agency.coupons.size &
      ".actions .delete" #> ClearNodesIf(agency.customers.size > 0) &
      ".actions .delete [onclick]" #> Confirm(s"Delete ${agency.name}? This will delete all members and coupons.",
        ajaxInvoke(deleteAgency(agency) _)
      ) &
      ".actions .sales-export [href]" #> Agencies.salesDataExportMenu.loc.calcDefaultHref &
      ".actions .cancellation-export [href]" #> Agencies.cancellationExportMenu.loc.calcDefaultHref &
      ".actions .total-sales-export [href]" #> Agencies.totalSalesExportMenu.loc.calcDefaultHref &
      ".actions .month-to-date-sales-export [href]" #> Agencies.monthToDateExportMenu.loc.calcDefaultHref
    }
  }
}



