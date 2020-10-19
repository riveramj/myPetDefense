package com.mypetdefense.service

import com.mypetdefense.model._
import dispatch._
import me.frmr.stripe.{StripeExecutor, Coupon => StripeCoupon}
import net.liftweb.common._
import net.liftweb.util.Helpers.tryo
import net.liftweb.util.Props

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure => TryFail, Success => TrySuccess, _}

object CouponService extends Loggable {
  val stripeSecretKey: String    = Props.get("secret.key") openOr ""
  implicit val e: StripeExecutor = new StripeExecutor(stripeSecretKey)

  def convertedString(possibleNumber: String): Int = {
    tryo(possibleNumber.trim().toInt).openOr(0)
  }

  def createStripeCoupon(
      couponCode: String,
      freeMonths: Int,
      percentOff: Int,
      dollarOff: Int
  ): Future[Box[StripeCoupon]] = {

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

  def createCoupon(
      couponCode: String,
      agency: Box[Agency],
      rawMonthCount: String,
      rawPercentOff: String,
      rawDollarOff: String
  ): Box[Coupon] = {

    val monthCount = convertedString(rawMonthCount)
    val percentOff = convertedString(rawPercentOff)
    val dollarOff  = convertedString(rawDollarOff)

    val newStripeCoupon = createStripeCoupon(
      couponCode,
      monthCount,
      percentOff,
      dollarOff
    )

    Try(Await.result(newStripeCoupon, new DurationInt(3).seconds)) match {
      case TrySuccess(Full(_)) =>
        Full(
          Coupon.createNewCoupon(
            couponCode,
            agency,
            monthCount,
            percentOff,
            dollarOff
          )
        )

      case TrySuccess(stripeFailure) =>
        logger.error("create coupon failed with stipe error: " + stripeFailure)
        Empty

      case TryFail(throwable: Throwable) =>
        logger.error("create coupon failed with other error: " + throwable)
        Empty
    }
  }

  def deleteCoupon(coupon: Coupon): Box[Coupon] = {
    val tryDelete = StripeCoupon.delete(coupon.couponCode.get)

    Try(Await.result(tryDelete, new DurationInt(3).seconds)) match {
      case TrySuccess(Full(_)) =>
        coupon.delete_!
        Full(coupon)

      case TrySuccess(stripeFailure) =>
        logger.error("create customer failed with: " + stripeFailure)
        logger.error(s"attempting to delete $coupon locally")

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
