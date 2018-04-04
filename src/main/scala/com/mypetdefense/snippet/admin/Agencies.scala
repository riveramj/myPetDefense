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

  def exportTotalSalesResponse: Box[LiftResponse] = {
    totalSalesExportMenu.currentValue flatMap { name =>
      ReportingService.exportTotalSales(name)
    } 
  }

  def exportRawSalesResponse: Box[LiftResponse] = {
    salesDataExportMenu.currentValue flatMap { name =>
      ReportingService.exportRawSales(name)
    } 
  }

  def exportMonthToDateSalesResponse: Box[LiftResponse] = {
    monthToDateExportMenu.currentValue flatMap { name =>
      ReportingService.exportMonthToDateSales(name)
    } 
  }

  def exportCancellationDataResponse: Box[LiftResponse] = {
    cancellationExportMenu.currentValue flatMap { name =>
      ReportingService.exportCancellationData(name)
    } 
  }

  val menu = Menu.i("Agencies") / "admin" / "agencies" >>
    adminUser >>
    loggedIn

    val totalSalesExportMenu = Menu.param[String](
      "Export Total Sales",
      "Export Total Sales",
      Full(_),
      string => string
    ) / "admin" / "agencies" / "month-year-gross-sales.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(exportTotalSalesResponse _)

  val salesDataExportMenu = Menu.param[String](
      "Export Gross Sales",
      "Export Gross Sales",
      Full(_),
      string => string
    ) / "admin" / "agencies" / "raw-sales.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(exportRawSalesResponse _)

  val monthToDateExportMenu = Menu.param[String](
      "Export Month to Date Sales",
      "Export Month to Date Sales",
      Full(_),
      string => string
    ) / "admin" / "agencies" / "mtd-sales.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(exportMonthToDateSalesResponse _)

  val cancellationExportMenu = Menu.param[String](
      "Export Cancellation Data",
      "Export Cancellation Data",
      Full(_),
      string => string
    ) / "admin" / "agencies" / "cancellation-data.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(exportCancellationDataResponse _)
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
      ".actions .sales-export [href]" #> Agencies.salesDataExportMenu.calcHref(agency.name.get) &
      ".actions .cancellation-export [href]" #> Agencies.cancellationExportMenu.calcHref(agency.name.get) &
      ".actions .total-sales-export [href]" #> Agencies.totalSalesExportMenu.calcHref(agency.name.get) &
      ".actions .month-to-date-sales-export [href]" #> Agencies.monthToDateExportMenu.calcHref(agency.name.get)
    }
  }
}



