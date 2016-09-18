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

  def freeMonthsConverted(freeMonths: String) = {
    tryo(freeMonths.trim().toInt).openOr(0)
  }

  def createStripeCoupon(couponCode: String, freeMonths: String, agency: Box[Agency]) = {
    StripeCoupon.create(
      id = Some(couponCode),
      duration = "repeating",
      percentOff = Some(100),
      durationInMonths = Some(freeMonthsConverted(freeMonths))
    )
  }

  def createCoupon(couponCode: String, freeMonths: String, agency: Box[Agency]): Box[Coupon] = {

    val newStripeCoupon = createStripeCoupon(couponCode, freeMonths, agency)

    Try(Await.result(newStripeCoupon, new DurationInt(5).seconds)) match {
      case TrySuccess(Full(newCoupon)) =>
        Full(Coupon.createNewCoupon(couponCode, freeMonthsConverted(freeMonths), agency))

      case TrySuccess(stripeFailure) =>
        logger.error("create coupon failed with 1: " + stripeFailure)
        Empty

      case TryFail(throwable: Throwable) =>
        logger.error("create coupon failed with 2: " + throwable)
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
