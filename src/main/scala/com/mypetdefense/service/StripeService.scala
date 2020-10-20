package com.mypetdefense.service

import com.mypetdefense.model.Coupon
import dispatch.Future
import me.frmr.stripe.{Customer, StripeExecutor}
import net.liftweb.common.Box
import net.liftweb.util.Props

object StripeService {
  val stripeSecretKey: String    = Props.get("secret.key") openOr ""
  implicit val e: StripeExecutor = new StripeExecutor(stripeSecretKey)

  def createStripeCustomer(
      coupon: Box[Coupon],
      email: String,
      stripeToken: String,
      pennyCount: Int,
      taxRate: Double
  ): Future[Box[Customer]] = {
    val couponId = coupon.map(_.couponCode.get)
    if (couponId.isEmpty) {
      Customer.create(
        email = Some(email),
        card = Some(stripeToken),
        plan = Some("pennyProduct"),
        quantity = Some(pennyCount),
        taxPercent = Some(taxRate)
      )
    } else {
      Customer.create(
        email = Some(email),
        card = Some(stripeToken),
        plan = Some("pennyProduct"),
        quantity = Some(pennyCount),
        taxPercent = Some(taxRate),
        coupon = couponId
      )
    }
  }
}
