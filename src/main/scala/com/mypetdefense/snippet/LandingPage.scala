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

  val cold5k = Menu.i("Dog Gone 5k") / "cold5k" >>
    TemplateBox(() => Templates("landing" :: "landing" :: Nil))
  
  val doggiePalooza = Menu.i("Doggie-Palooza") / "palooza" >>
    TemplateBox(() => Templates("landing" :: "landing" :: Nil))

  val woofstock = Menu.i("Woofstock 2018") / "woofstock" >>
    TemplateBox(() => Templates("landing" :: "landing" :: Nil))
}

class LandingPage extends Loggable {
  import PetFlowChoices._

  val path = S.request.map(_.uri).openOr("").drop(1)

  val (modalTitle, modalOffer) = path match {
    case "2free" => 
      val possibleCoupon = Coupon.find(By(Coupon.couponCode, "2free"))
      PetFlowChoices.coupon(possibleCoupon)
      PetFlowChoices.priceCode(Full("default"))
      ("","")
      
    case "3free" => 
      val possibleCoupon = Coupon.find(By(Coupon.couponCode, "3free"))
      PetFlowChoices.coupon(possibleCoupon)
      PetFlowChoices.priceCode(Full("default"))
      ("","")

    case "cold5k" => 
      val possibleCoupon = Coupon.find(By(Coupon.couponCode, "cold5k"))
      PetFlowChoices.coupon(possibleCoupon)
      PetFlowChoices.priceCode(Full("cold5k"))
      ("Dog Gone Cold 5k Discount", "Great job out there at the race! Click below to get your disconted pricing at $9.99 a month.")

    case "palooza" => 
      val possibleCoupon = Coupon.find(By(Coupon.couponCode, "palooza"))
      PetFlowChoices.coupon(possibleCoupon)
      PetFlowChoices.priceCode(Full("default"))
      ("Doggie-Palooza Discount","Hope you enjoyed your day at the park! Two free months has been added to your cart.")

    case "woofstock" =>
      val possibleCoupon = Coupon.find(By(Coupon.couponCode, "woofstock"))
      PetFlowChoices.coupon(possibleCoupon)
      PetFlowChoices.priceCode(Full("default"))
      ("Woofstock 2018 Discount","Hope you enjoyed your day at Woofstock! Two free months has been added to your cart.")
  }

  val monthCount = PetFlowChoices.coupon.is.map(_.freeMonths.get).openOr(0)
  val couponCode = PetFlowChoices.coupon.is.map(_.couponCode.get).openOr("")

  def render = {
    ".coupon-code *" #> couponCode &
    ".applied-months *" #> s" ${monthCount} months free!" &
    ".low-price *" #> { if (couponCode == "cold5k") "$10" else "$13" } &
    ".modal-title *" #> modalTitle &
    ".modal-offer *" #> modalOffer &
    ".modal [class+]" #> { if (modalTitle == "") "" else "active" }
  }
}
