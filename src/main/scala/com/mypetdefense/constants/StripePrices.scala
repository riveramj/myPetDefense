package com.mypetdefense.constants

import net.liftweb.util.Props

object StripePrices {
  final case class ProductBox(priceId: String, monthlyCharge: BigDecimal)

  object Cat {
    val BasicBox: ProductBox = ProductBox(stripePricesBoxesCatBasicId, BigDecimal("12.99"))
  }

  object Dog {
    val BasicBox: ProductBox = ProductBox(stripePricesBoxesDogBasicId, BigDecimal("12.99"))

    object HealthAndWellnessBox {
      val Small: ProductBox  = ProductBox(stripePricesBoxesDogHwSmallId, BigDecimal("24.99"))
      val Medium: ProductBox = ProductBox(stripePricesBoxesDogHwMedXlId, BigDecimal("27.99"))
      val Large: ProductBox  = Medium
      val XLarge: ProductBox = Medium
    }
  }

  private val stripePricesBoxesCatBasicId   = priceId("stripe.prices.boxes.cat.basic.id")
  private val stripePricesBoxesDogBasicId   = priceId("stripe.prices.boxes.dog.basic.id")
  private val stripePricesBoxesDogHwSmallId = priceId("stripe.prices.boxes.dog.hw.small.id")
  private val stripePricesBoxesDogHwMedXlId = priceId("stripe.prices.boxes.dog.hw.med.xl.id")

  private def priceId(name: String): String =
    Props.get(name).openOrThrowException(s"Price ID '$name' is required")
}
