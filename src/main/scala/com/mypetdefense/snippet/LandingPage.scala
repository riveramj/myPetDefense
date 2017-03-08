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

  val fetchLanding = Menu.i("Fetch!") / "fetch" >>
    TemplateBox(() => Templates("landing" :: "landing" :: Nil))
}

class LandingPage extends Loggable {
  import PetFlowChoices._

  val possibleTwoFreeCoupon = Coupon.find(By(Coupon.couponCode, "fetch2free"))
  coupon(possibleTwoFreeCoupon)

  def render = {
    "#foo" #> ""
  }

}
