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
import com.mypetdefense.service.CouponService
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

  val salesDataExportMenu = Menu.i("Export Gross Sales") / "admin" / "agencies" / "month-year-gross-sales.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(exportGrossSales _)


  def shipmentAmountPaid(shipment: Shipment) = {
    val amountPaid = tryo(shipment.amountPaid.get.toDouble).getOrElse(0D)
    val taxesPaid = tryo(shipment.taxPaid.get.toDouble).getOrElse(0D) 
    amountPaid - taxesPaid
  }

  def exportGrossSales: Box[LiftResponse] = {
    val currentDate = LocalDateTime.now()
    val yearMonth = currentDate.format(DateTimeFormatter.ofPattern("MMMM yyyy", Locale.ENGLISH))
    val fileNameYearMonth = currentDate.format(DateTimeFormatter.ofPattern("MMMyyyy", Locale.ENGLISH))
    val year = currentDate.format(DateTimeFormatter.ofPattern("yyyy", Locale.ENGLISH))

    val headers = "Year" :: "Month" :: "Date" :: "Customer Id" :: "Customer Name" :: "Amount" :: "Call Agent" :: "Commision" :: Nil

    val csvRows: List[List[String]] = {
      for {
        agency <- Agency.find(By(Agency.name, "TPP")).toList
        customer <- agency.customers.toList
        subscription <- customer.subscription
        shipment <- subscription.shipments.toList.sortBy(_.dateProcessed.get.getTime)
      } yield {
        val processDate = shipment.dateProcessed.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()

        val amountPaid = shipmentAmountPaid(shipment)
        val commision = amountPaid * .35 

        processDate.getYear.toString ::
        processDate.getMonth.toString ::
        processDate.toString ::
        customer.userId.toString ::
        customer.name ::
        s"$$${amountPaid}" ::
        customer.salesAgent.obj.map(_.name).openOr("") ::
        f"$$$commision%2.2f" ::
        Nil
      }
    }

    val spacerRow = List(List(","))

    val resultingCsv = (List(headers) ++ csvRows).map(_.mkString(",")).mkString("\n")

    val fileName = s"salesData-${fileNameYearMonth}.csv"

    val file = "filename=\"" + fileName + "\""

    Some(new InMemoryResponse(
      resultingCsv.getBytes("UTF-8"),
      List(
        "Content-Type" -> "binary/octet-stream",
        "Content-Disposition" -> s"attachment; ${file}"
      ),
    Nil,
    200
  ))
  }
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
      ".actions .sales-export [href]" #> Agencies.salesDataExportMenu.loc.calcDefaultHref
    }
  }
}



