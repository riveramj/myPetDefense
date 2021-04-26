package com.mypetdefense.constants

import net.liftweb.util.Props

object StripeProductsPrices {
  final case class ProductBox(productId: String, priceId: String, monthlyCharge: BigDecimal)

  object Cat {
    val BasicBox: ProductBox = ProductBox("", stripePricesBoxesCatBasicId, BigDecimal("12.99"))
  }

  object Dog {
    val BasicBox: ProductBox = ProductBox("", stripePricesBoxesDogBasicId, BigDecimal("12.99"))

    object HealthAndWellnessBox {
      val Small: ProductBox  = ProductBox(stripeProductBoxDogHwSmallId, stripePriceBoxDogHwSmallId, BigDecimal("24.99"))
      val Medium: ProductBox = ProductBox(stripeProductBoxDogHwMediumId, stripePriceBoxDogHwMediumId, BigDecimal("27.99"))
      val Large: ProductBox  = ProductBox(stripeProductBoxDogHwLargeId, stripePriceBoxDogHwLargeId, BigDecimal("27.99"))
      val XLarge: ProductBox = ProductBox(stripeProductBoxDogHwXLargeId, stripePriceBoxDogHwXLargeId, BigDecimal("27.99"))
    }
  }

  private val stripePricesBoxesCatBasicId   = stripeId("stripe.prices.boxes.cat.basic.id")
  private val stripePricesBoxesDogBasicId   = stripeId("stripe.prices.boxes.dog.basic.id")

  private val stripePriceBoxDogHwSmallId = stripeId("stripe.price.box.dog.hw.small.id")
  private val stripeProductBoxDogHwSmallId = stripeId("stripe.product.box.dog.hw.small.id")

  private val stripePriceBoxDogHwMediumId = stripeId("stripe.price.box.dog.hw.medium.id")
  private val stripeProductBoxDogHwMediumId = stripeId("stripe.product.box.dog.hw.medium.id")

  private val stripePriceBoxDogHwLargeId = stripeId("stripe.price.box.dog.hw.large.id")
  private val stripeProductBoxDogHwLargeId = stripeId("stripe.product.box.dog.hw.large.id")

  private val stripePriceBoxDogHwXLargeId = stripeId("stripe.price.box.dog.hw.xlarge.id")
  private val stripeProductBoxDogHwXLargeId = stripeId("stripe.product.box.dog.hw.xlarge.id")

  private def stripeId(name: String): String =
    Props.get(name).openOrThrowException(s"ID '$name' is required")
}
