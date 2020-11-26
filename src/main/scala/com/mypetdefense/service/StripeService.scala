package com.mypetdefense.service

import com.mypetdefense.model.Coupon
import com.mypetdefense.util.StripeHelper._
import com.stripe.model.{Customer, TaxRate, Subscription => StripeSubscription}
import com.stripe.param._
import net.liftweb.common.{Box, Empty}

import scala.collection.JavaConverters._

object StripeService {

  def createStripeCustomer(
      email: String,
      stripeToken: String,
      plan: String,
      quantity: Int,
      taxRate: Double,
      coupon: Box[Coupon] = Empty
  ): Box[Customer] = {

    val couponId = coupon.map(_.couponCode.get)

    def createCustomer: Customer =
      Customer.create(
        CustomerCreateParams.builder
          .setEmail(email)
          .setSource(stripeToken)
          .whenDefined(couponId)(_.setCoupon)
          .build
      )

    val createSubscription: ((Customer, TaxRate)) => Box[StripeSubscription] = {
      case (customer, taxRate) =>
        createStripeSubscription(customer, taxRate, plan, quantity)
    }

    BoxedApiRequest(createCustomer)
      .zipBox(retrieveOrCreateTaxRate(taxRate))
      .andThenBoxLeft(createSubscription)
      .map(_._1)
      .toBox
  }

  def createStripeSubscription(
      customer: Customer,
      taxRate: TaxRate,
      plan: String,
      quantity: Int,
      coupon: Box[String] = Empty
  ): Box[StripeSubscription] =
    Box.tryo {
      StripeSubscription.create(
        SubscriptionCreateParams.builder
          .setCustomer(customer.getId)
          .whenDefined(coupon)(_.setCoupon)
          .addDefaultTaxRate(taxRate.getId)
          .addItem(
            SubscriptionCreateParams.Item.builder
              .setPlan(plan) // TODO: probably should migrate to prices
              .setQuantity(quantity)
              .build
          )
          .build
      )
    }

  def retrieveOrCreateTaxRate(taxRate: BigDecimal): Box[TaxRate] = {
    val maybeTaxRate: Box[TaxRate] = Box.tryo {
      val params =
        TaxRateListParams.builder
          .setActive(true)
          .setLimit(100L)
          .build

      TaxRate
        .list(params)
        .getData
        .asScala
        .filter(_.getPercentage == taxRate.bigDecimal)
    }.flatMap(_.headOption)

    def createTaxRate: Box[TaxRate] = Box.tryo {
      TaxRate.create(
        TaxRateCreateParams.builder
          .setDisplayName("Tax")
          .setPercentage(taxRate.bigDecimal)
          .setInclusive(false)
          .build
      )
    }

    maybeTaxRate or createTaxRate
  }

  def replaceTaxRateOnSubscription(
      subscriptionId: String,
      taxRate: BigDecimal
  ): Box[StripeSubscription] =
    for {
      txr <- retrieveOrCreateTaxRate(taxRate)
      sub <- updateSubscription(
              subscriptionId,
              SubscriptionUpdateParams.builder
                .addDefaultTaxRate(txr.getId)
                .build
            )
    } yield sub

}
