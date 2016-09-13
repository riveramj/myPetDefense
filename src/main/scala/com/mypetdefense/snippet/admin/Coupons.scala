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

import me.frmr.stripe.{StripeExecutor, Customer, Coupon => StripeCoupon}
import scala.util.{Failure => TryFail, Success => TrySuccess, _}

import scala.concurrent.Await
import scala.concurrent.duration._

import dispatch._, Defaults._

object Coupons extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Coupons") / "admin" / "coupons" 
}

class Coupons extends Loggable {
  val stripeSecretKey = Props.get("secret.key") openOr ""
  implicit val e = new StripeExecutor(stripeSecretKey)

  val coupons = Coupon.findAll()
  val allAgencies = Agency.findAll()

  var codeName = ""
  var freeMonths = ""
  var chosenAgency: Box[Agency] = Empty

  def createStripeCoupon = {
    StripeCoupon.create(
      id = Some(codeName),
      duration = "repeating",
      percentOff = Some(100),
      durationInMonths = Some(freeMonthsConverted)
    )
  }

  def freeMonthsConverted = {
    tryo(freeMonths.trim().toInt).openOr(0)
  }
  
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
      checkNumber(freeMonths.trim(), "#free-months")
    ).flatten

    if(validateFields.isEmpty) {
      val newStripeCoupon = createStripeCoupon

      Try(Await.result(newStripeCoupon, new DurationInt(3).seconds)) match {
        case TrySuccess(Full(coupon)) =>
          Coupon.createCoupon(
            codeName.toLowerCase().trim(),
            freeMonthsConverted,
            chosenAgency
          )

          S.redirectTo(Coupons.menu.loc.calcDefaultHref)

        case TrySuccess(stripeFailure) =>
          logger.error("create customer failed with: " + stripeFailure)
          Alert("An error has occured. Please try again.")
        
        case TryFail(throwable: Throwable) =>
          logger.error("create customer failed with: " + throwable)
          Alert("An error has occured. Please try again.")
      }
    } else {
      validateFields.foldLeft(Noop)(_ & _)
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
      ".agency *" #> coupon.agency.obj.map(_.name.get)
    }
  }
}
