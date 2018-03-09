package com.mypetdefense.service

import net.liftweb._ 
  import common._
  import util.Helpers.tryo
  import json._
  import util.Props

import me.frmr.stripe.{StripeExecutor, Customer, Coupon => StripeCoupon}
import scala.util.{Failure => TryFail, Success => TrySuccess, _}

import com.mypetdefense.model._

import scala.concurrent.Await
import scala.concurrent.duration._

import dispatch._, Defaults._

object CouponService extends Loggable {
  val stripeSecretKey = Props.get("secret.key") openOr ""
  implicit val e = new StripeExecutor(stripeSecretKey)

  def convertedString(possibleNumber: String) = {
    tryo(possibleNumber.trim().toInt).openOr(0)
  }

  def createStripeCoupon(couponCode: String, agency: Box[Agency], freeMonths: Int, percentOff: Int, dollarOff: Int) = {

    val (durationType, durationMonths) = {
      if (freeMonths == 0)
        ("forever", None)
      else
        ("repeating", Some(freeMonths))
    }

    if (dollarOff > 0) {
      StripeCoupon.create(
        id = Some(couponCode),
        duration = durationType,
        amountOff = Some(dollarOff * 100),
        currency = Some("USD"),
        durationInMonths = durationMonths
      )
    } else {
      StripeCoupon.create(
        id = Some(couponCode),
        duration = durationType,
        percentOff = Some(percentOff),
        durationInMonths = durationMonths
      )
    }
  }

  def createCoupon(couponCode: String, agency: Box[Agency], rawFreeMonths: String, rawPercentOff: String, rawDollarOff: String): Box[Coupon] = {

    val freeMonths = convertedString(rawFreeMonths)
    val percentOff = convertedString(rawPercentOff)
    val dollarOff = convertedString(rawDollarOff)

    val newStripeCoupon = createStripeCoupon(
      couponCode,
      agency,
      freeMonths,
      percentOff,
      dollarOff
    )

    Try(Await.result(newStripeCoupon, new DurationInt(3).seconds)) match {
      case TrySuccess(Full(newCoupon)) =>
        Full(Coupon.createNewCoupon(
          couponCode,
          agency,
          freeMonths,
          percentOff,
          dollarOff
        ))

      case TrySuccess(stripeFailure) =>
        logger.error("create coupon failed with stipe error: " + stripeFailure)
        Empty

      case TryFail(throwable: Throwable) =>
        logger.error("create coupon failed with other error: " + throwable)
        Empty
    }
  }

  def deleteCoupon(coupon: Coupon) = {
    val tryDelete = StripeCoupon.delete(coupon.couponCode.get)

    Try(Await.result(tryDelete, new DurationInt(3).seconds)) match {
      case TrySuccess(Full(_)) =>
        coupon.delete_!
        Full(coupon)

      case TrySuccess(stripeFailure) =>
        logger.error("create customer failed with: " + stripeFailure)
        logger.error(s"attempting to delete ${coupon} locally")
      
        if (coupon.delete_!)
          Full(coupon)
        else
          Empty

      case TryFail(throwable: Throwable) =>
        logger.error("create customer failed with: " + throwable)
        Empty
    }
  }
}
