package com.mypetdefense.snippet 

import net.liftweb._
  import http.SHtml._
  import util._
  import util.Helpers._
  import common._
  import util.ClearClearable
  import http._
  import mapper.{By, NullRef}
  import js._
  import JsCmds._

import com.mypetdefense.service._
  import ValidationService._
  import PetFlowChoices._

import com.mypetdefense.model._

import java.util.Date
import java.text.SimpleDateFormat

object LandingPage extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val landing2Free = Menu.i("2 Months Free!") / "2free" >>
    TemplateBox(() => Templates("landing" :: "landing" :: Nil))
  
  val landing3Free = Menu.i("3 Months Free!") / "3free" >>
    TemplateBox(() => Templates("landing" :: "landing" :: Nil))

  val cold5k = Menu.i("Dog Gone 5k") / "5k" >>
    TemplateBox(() => Templates("landing" :: "landing" :: Nil))
}

class LandingPage extends Loggable {
  import PetFlowChoices._

  val path = S.request.map(_.uri).openOr("").drop(1)

  val (possibleCoupon, monthCount) = path match {
    case "2free" => (Coupon.find(By(Coupon.couponCode, "2free")), 2)
    case "3free" => (Coupon.find(By(Coupon.couponCode, "3free")), 3)
    case "cold5k" => (Coupon.find(By(Coupon.couponCode, "cold5k")), 2)
  }

  PetFlowChoices.coupon(possibleCoupon)


  def render = {
    ".coupon-code *" #> path &
    ".applied-months *" #> s" ${monthCount} months free!" &
    ".low-price *" #> { if (path == "cold5k") "$10" else "$13" }
  }

}
