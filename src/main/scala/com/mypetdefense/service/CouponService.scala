package com.mypetdefense.service

import com.mypetdefense.model._
import com.mypetdefense.util.StripeHelper.{deleteCoupon => deleteStripeCoupon, _}
import com.stripe.model.{Coupon => StripeCoupon}
import net.liftweb.common._
import net.liftweb.util.Helpers.tryo

object CouponService extends Loggable {

  def convertedString(possibleNumber: String): Int = {
    tryo(possibleNumber.trim().toInt).openOr(0)
  }

  def createStripeCoupon(
      couponCode: String,
      freeMonths: Int,
      percentOff: Int,
      dollarOff: Int
  ): Box[StripeCoupon] = {

    val (durationType, durationMonths) = {
      if (freeMonths == 0)
        ("forever", None)
      else  if (freeMonths == 1)
        ("once", None)
      else
        ("repeating", Some(freeMonths))
    }

    val params =
      if (dollarOff > 0)
        ParamsMap(
          "id" --> couponCode,
          "duration" --> durationType,
          "amount_off" --> dollarOff * 100,
          "currency" --> "USD",
          "duration_in_months" -?> durationMonths
        )
      else
        ParamsMap(
          "id" --> couponCode,
          "duration" --> durationType,
          "percent_off" --> percentOff,
          "duration_in_months" -?> durationMonths
        )

    Box.tryo { StripeCoupon.create(params) }
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

    newStripeCoupon match {
      case Full(_) =>
        Full(
          Coupon.createNewCoupon(
            couponCode,
            agency,
            monthCount,
            percentOff,
            dollarOff
          )
        )

      case stripeFailure =>
        logger.error("create coupon failed with stripe error: " + stripeFailure)
        Empty
    }
  }

  def deleteCoupon(coupon: Coupon): Box[Coupon] = {
    deleteStripeCoupon(coupon.couponCode.get) match {
      case Full(_) =>
        coupon.delete_!
        Full(coupon)

      case stripeFailure =>
        logger.error("create customer failed with: " + stripeFailure)
        logger.error(s"attempting to delete $coupon locally")

        if (coupon.delete_!)
          Full(coupon)
        else
          Empty
    }
  }
}
