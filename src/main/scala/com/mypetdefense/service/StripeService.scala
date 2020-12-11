package com.mypetdefense.service

import com.mypetdefense.model.Coupon
import com.mypetdefense.util.StripeHelper._
import com.stripe.model.Customer
import net.liftweb.common.Box

object StripeService {
  def createStripeCustomer(
      coupon: Box[Coupon],
      email: String,
      stripeToken: String,
      pennyCount: Int,
      taxRate: Double
  ): Box[Customer] = {

    val couponId = coupon.map(_.couponCode.get)

    val params = ParamsMap(
      "email" --> email,
      "card" --> stripeToken,
      "plan" --> "pennyProduct",
      "quantity" --> pennyCount,
      "tax_percent" --> taxRate,
      "coupon" -?> couponId
    )

    Box.tryo { Customer.create(params) }
  }
}
