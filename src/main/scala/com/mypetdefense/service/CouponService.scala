package com.mypetdefense.service

import com.mypetdefense.model._
import com.mypetdefense.service.{StripeBoxAdapter => Stripe}
import com.mypetdefense.util.StripeHelper._
import com.stripe.param.CouponCreateParams
import net.liftweb.common._
import net.liftweb.util.Helpers.tryo

object CouponService extends Loggable {

  def convertedString(possibleNumber: String): Int = {
    tryo(possibleNumber.trim().toInt).openOr(0)
  }

  def createStripeCoupon(
      couponCode: String,
      freeMonths: Long,
      percentOff: BigDecimal,
      dollarOff: Int
  ): Box[Stripe.Coupon] = {

    val (durationType, durationMonths) = {
      import CouponCreateParams.Duration._
      if (freeMonths == 0L)
        (FOREVER, None)
      else if (freeMonths == 1L)
        (ONCE, None)
      else
        (REPEATING, Some(freeMonths))
    }

    val params =
      CouponCreateParams.builder
        .setId(couponCode)
        .setDuration(durationType)
        .whenDefinedC(durationMonths)(_.setDurationInMonths)
        .when(dollarOff > 0) { b =>
          b.setAmountOff(dollarOff * 100)
          b.setCurrency("USD")
        } {
          _.setPercentOff(percentOff.bigDecimal)
        }
        .build

    Stripe.Coupon.create(params)
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
    StripeFacade.Coupon.delete(coupon.couponCode.get) match {
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
