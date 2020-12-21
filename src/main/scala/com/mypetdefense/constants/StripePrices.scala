package com.mypetdefense.constants

import net.liftweb.util.Props

object StripePrices {
  final case class ProductBox(priceId: String, monthlyCharge: BigDecimal)

  object Cat {
    val BasicBox: ProductBox = ProductBox(`stripe.prices.boxes.cat.basic.id`, BigDecimal("12.99"))
  }

  object Dog {
    val BasicBox: ProductBox = ProductBox(`stripe.prices.boxes.dog.basic.id`, BigDecimal("12.99"))

    object HealthAndWellnessBox {
      val Small: ProductBox  = ProductBox(`stripe.prices.boxes.dog.hwsmall.id`, BigDecimal("24.99"))
      val Medium: ProductBox = ProductBox(`stripe.prices.boxes.dog.hwmedxl.id`, BigDecimal("27.99"))
      val Large: ProductBox  = Medium
      val XLarge: ProductBox = Medium
    }
  }

  private val `stripe.prices.boxes.cat.basic.id`   = priceId("stripe.prices.boxes.cat.basic.id")
  private val `stripe.prices.boxes.dog.basic.id`   = priceId("stripe.prices.boxes.dog.basic.id")
  private val `stripe.prices.boxes.dog.hwsmall.id` = priceId("stripe.prices.boxes.dog.hwsmall.id")
  private val `stripe.prices.boxes.dog.hwmedxl.id` = priceId("stripe.prices.boxes.dog.hwmedxl.id")

  private def priceId(name: String): String =
    Props.get(name).openOrThrowException(s"Price ID '$name' is required")
}
