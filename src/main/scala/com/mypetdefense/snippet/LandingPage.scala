package com.mypetdefense.snippet

import com.mypetdefense.model._
import com.mypetdefense.service._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._
import net.liftweb.util._

object LandingPage extends Loggable {
  import net.liftweb.sitemap._
  import Loc._

  val landing2Free: Menu.Menuable = Menu.i("2 Months Free!") / "2free" >>
    TemplateBox(() => Templates("landing" :: "landing" :: Nil))

  val landing3Free: Menu.Menuable = Menu.i("3 Months Free!") / "3free" >>
    TemplateBox(() => Templates("landing" :: "landing" :: Nil))

  val cold5k: Menu.Menuable = Menu.i("Dog Gone 5k") / "cold5k" >>
    TemplateBox(() => Templates("landing" :: "landing" :: Nil))

  val doggiePalooza: Menu.Menuable = Menu.i("Doggie-Palooza") / "palooza" >>
    TemplateBox(() => Templates("landing" :: "landing" :: Nil))

  val woofstock: Menu.Menuable = Menu.i("Woofstock 2018") / "woofstock" >>
    TemplateBox(() => Templates("landing" :: "landing" :: Nil))

  val olympics: Menu.Menuable = Menu.i("Rescue Dog Olympics") / "olympics" >>
    TemplateBox(() => Templates("landing" :: "landing" :: Nil))

  val atlantaExpo: Menu.Menuable = Menu.i("Atlanta Expo") / "atlanta" >>
    TemplateBox(() => Templates("landing" :: "landing" :: Nil))

  val firstMonthFree: Menu.Menuable = Menu.i("First Month Free") / "firstmonthfree" >>
    TemplateBox(() => Templates("landing" :: "landing" :: Nil))
}

class LandingPage extends Loggable {
  val path: String = S.request.map(_.uri).openOr("").drop(1)

  val (modalTitle, modalOffer) = path match {
    case "2free" =>
      val possibleCoupon = Coupon.find(By(Coupon.couponCode, "2free"))
      PetFlowChoices.coupon(possibleCoupon)
      PetFlowChoices.priceCode(Full(Price.defaultPriceCode))
      ("", "")

    case "3free" =>
      val possibleCoupon = Coupon.find(By(Coupon.couponCode, "3free"))
      PetFlowChoices.coupon(possibleCoupon)
      PetFlowChoices.priceCode(Full(Price.defaultPriceCode))
      ("", "")

    case "cold5k" =>
      val possibleCoupon = Coupon.find(By(Coupon.couponCode, "cold5k"))
      PetFlowChoices.coupon(possibleCoupon)
      PetFlowChoices.priceCode(Full("cold5k"))
      (
        "Dog Gone Cold 5k Discount",
        "Great job out there at the race! Click below to get your disconted pricing at $9.99 a month."
      )

    case "palooza" =>
      val possibleCoupon = Coupon.find(By(Coupon.couponCode, "palooza"))
      PetFlowChoices.coupon(possibleCoupon)
      PetFlowChoices.priceCode(Full(Price.defaultPriceCode))
      (
        "Doggie-Palooza Discount",
        "Hope you enjoyed your day at the park! Two free months has been added to your cart."
      )

    case "woofstock" =>
      val possibleCoupon = Coupon.find(By(Coupon.couponCode, "woofstock"))
      PetFlowChoices.coupon(possibleCoupon)
      PetFlowChoices.priceCode(Full(Price.defaultPriceCode))
      (
        "Woofstock 2018 Discount",
        "Hope you enjoyed your day at Woofstock! Two free months has been added to your cart."
      )

    case "olympics" =>
      val possibleCoupon = Coupon.find(By(Coupon.couponCode, "olympics"))
      PetFlowChoices.coupon(possibleCoupon)
      PetFlowChoices.priceCode(Full(Price.defaultPriceCode))
      (
        "Rescue Dog Olympics",
        "Hope you enjoyed your day at the Olympics! Two free months has been added to your cart."
      )

    case "atlanta" =>
      val possibleCoupon = Coupon.find(By(Coupon.couponCode, "atlanta"))
      PetFlowChoices.coupon(possibleCoupon)
      PetFlowChoices.priceCode(Full(Price.defaultPriceCode))
      (
        "Atlata Pet Expo",
        "Hope you enjoyed your day at the show! Two free months has been added to your cart."
      )

    case "firstmonthfree" =>
      val possibleCoupon = Coupon.find(By(Coupon.couponCode, "firstmonthfree"))
      PetFlowChoices.coupon(possibleCoupon)
      PetFlowChoices.priceCode(Full(Price.defaultPriceCode))
      (
        "It was nice to see you!",
        "Thank you for stopping by to talk with us! Your first free month has been added to your cart."
      )
  }

  val monthCount: Int    = PetFlowChoices.coupon.is.map(_.numberOfMonths.get).openOr(0)
  val couponCode: String = PetFlowChoices.coupon.is.map(_.couponCode.get).openOr("")

  def render: CssBindFunc = {
    ".coupon-code *" #> couponCode &
      ".applied-months *" #> s" ${monthCount} months free!" &
      ".low-price *" #> { if (couponCode == "cold5k") "$10" else "$13" } &
      ".modal-title *" #> modalTitle &
      ".modal-offer *" #> modalOffer &
      ".modal [class+]" #> { if (modalTitle == "") "" else "active" }
  }
}
