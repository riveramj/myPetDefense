package com.mypetdefense.snippet
package admin

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util._
  import Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
  import js.JsCmds._
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service.CouponService
import com.mypetdefense.util.ClearNodesIf

object Coupons extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Coupons") / "admin" / "coupons" >>
    adminUser >>
    loggedIn
}

class Coupons extends Loggable {
  val coupons = Coupon.findAll()
  val allAgencies = Agency.findAll()

  var codeName = ""
  var freeMonths = ""
  var chosenAgency: Box[Agency] = Empty

  def agencyDropdown = {
    SHtml.selectObj(
      allAgencies.map(agency => (agency, agency.name.get)),
      chosenAgency,
      (agency: Agency) => chosenAgency = Full(agency)
    )
  }

  def createCoupon = {
    val validateFields = List(
      checkEmpty(codeName, "#code-name"),
      checkDuplicateCoupon(codeName, "#code-name"),
      checkNumber(freeMonths.trim(), "#free-months")
    ).flatten

    if (validateFields.isEmpty) {
      CouponService.createCoupon(
        codeName.toLowerCase().trim(),
        freeMonths,
        chosenAgency
      ) match { 
        case Full(coupon) =>
          S.redirectTo(Coupons.menu.loc.calcDefaultHref)

        case Empty | Failure(_,_,_) =>
          Alert("An error has occured. Please try again.")
      }
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def deleteCoupon(coupon: Coupon)() = {
    CouponService.deleteCoupon(coupon) match {
      case Full(_) =>
        S.redirectTo(Coupons.menu.loc.calcDefaultHref)
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".coupons [class+]" #> "current" &
    "#code-name" #> text(codeName, codeName = _) &
    "#free-months" #> text(freeMonths, freeMonths = _) &
    "#agency-container #agency-select" #> agencyDropdown &
    "#create-item" #> SHtml.ajaxSubmit("Create Coupon", () => createCoupon) &
    ".coupon" #> coupons.map { coupon =>
      ".code *" #> coupon.couponCode &
      ".months *" #> coupon.freeMonths &
      ".usage-count *" #> coupon.users.size &
      ".agency *" #> coupon.agency.obj.map(_.name.get) &
      ".actions .delete" #> ClearNodesIf(coupon.users.size > 0) &
      ".actions .delete [onclick]" #> Confirm(s"Delete ${coupon.couponCode}?",
        ajaxInvoke(deleteCoupon(coupon))
      )
    }
  }
}
