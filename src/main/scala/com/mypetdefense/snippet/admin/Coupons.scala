package com.mypetdefense.snippet
package admin

import net.liftweb.http.SHtml._
import net.liftweb.util._
import Helpers._
import net.liftweb.common._
import net.liftweb.http._
import js.JsCmds._

import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service.CouponService
import com.mypetdefense.util.ClearNodesIf
import net.liftweb.http.js.JsCmd

import scala.xml.{Elem, NodeSeq}

object Coupons extends Loggable {
  import net.liftweb.sitemap._
  import com.mypetdefense.util.Paths._

  val menu: Menu.Menuable = Menu.i("Coupons") / "admin" / "coupons" >>
    mpdAdmin >>
    loggedIn
}

class Coupons extends Loggable {
  val coupons: List[Coupon] = Coupon.findAll()
  val allAgencies: List[Agency] = Agency.findAll()

  var codeName = ""
  var monthCount = "0"
  var percentOff = "0"
  var dollarOff = "0"
  var chosenAgency: Box[Agency] = Empty

  def agencyDropdown: Elem = {
    SHtml.selectObj(
      allAgencies.map(agency => (agency, agency.name.get)),
      chosenAgency,
      (agency: Agency) => chosenAgency = Full(agency)
    )
  }

  def createCoupon: JsCmd = {
    val validateFields = (
      List(
        checkEmpty(codeName, "#code-name"),
        checkCouponValue(percentOff.trim(), "#percent-off"),
        checkCouponValue(monthCount.trim(), "#month-count"),
        checkDuplicateCoupon(codeName, "#code-name")
      ) ++ checkMonthPercentDollar(
        (monthCount.trim(), "#month-count"),
        (percentOff.trim(), "#percent-off"),
        (dollarOff.trim(), "#dollar-off")
      )).flatten

    if (validateFields.isEmpty) {
      CouponService.createCoupon(
        codeName.toLowerCase().trim(),
        chosenAgency,
        monthCount,
        percentOff,
        dollarOff
      ) match { 
        case Full(_) =>
          S.redirectTo(Coupons.menu.loc.calcDefaultHref)

        case Empty | Failure(_,_,_) =>
          Alert("An error has occurred. Please try again.")
      }
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def deleteCoupon(coupon: Coupon)(): Alert = {
    CouponService.deleteCoupon(coupon) match {
      case Full(_) =>
        S.redirectTo(Coupons.menu.loc.calcDefaultHref)
      case _ =>
        Alert("An error has occurred. Please try again.")
    }
  }

  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
    ".coupons [class+]" #> "current" &
    "#code-name" #> text(codeName, codeName = _) &
    "#month-count" #> text(monthCount, monthCount = _) &
    "#percent-off" #> text(percentOff, percentOff = _) &
    "#dollar-off" #> text(dollarOff, dollarOff = _) &
    "#agency-container #agency-select" #> agencyDropdown &
    "#create-item" #> SHtml.ajaxSubmit("Create Coupon", () => createCoupon) &
    ".coupon" #> coupons.map { coupon =>
      val couponPercent = {
        if (coupon.dollarOff.get > 0)
          "-"
        else if (coupon.percentOff.get == 0)
          "100%"
        else 
          s"${coupon.percentOff.get}%"
      }

      val couponDollar = {
        if (coupon.dollarOff.get > 0)
          s"$$${coupon.dollarOff.get}"
        else
          "-"
      }

      val monthCount = {
        if (coupon.numberOfMonths.get == 0)
          "-"
        else
          coupon.numberOfMonths.get.toString
      }

      ".code *" #> coupon.couponCode &
      ".months *" #> monthCount &
      ".percent-off *" #> couponPercent &
      ".dollar-off *" #> couponDollar &
      ".usage-count *" #> coupon.users.size &
      ".agency *" #> coupon.agency.obj.map(_.name.get) &
      ".actions .delete" #> ClearNodesIf(coupon.users.nonEmpty) &
      ".actions .delete [onclick]" #> Confirm(s"Delete ${coupon.couponCode}?",
        ajaxInvoke(deleteCoupon(coupon))
      )
    }
  }
}
