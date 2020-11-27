package com.mypetdefense.service

import com.mypetdefense.model.Coupon
import com.mypetdefense.service.{StripeBoxAdapter => Stripe}
import com.mypetdefense.util.StripeHelper._
import com.stripe.param._
import net.liftweb.common.{Box, Empty}

object StripeFacade {

  object Coupon {
    def delete(couponId: String): Box[Stripe.Coupon] =
      Stripe.Coupon.retrieve(couponId).flatMap(_.delete())
  }

  object Customer {
    def create(
        email: String,
        stripeToken: String,
        coupon: Box[Coupon] = Empty
    ): Box[Stripe.Customer] = {
      val couponId = coupon.map(_.couponCode.get)

      Stripe.Customer.create(
        CustomerCreateParams.builder
          .setEmail(email)
          .setSource(stripeToken)
          .whenDefined(couponId)(_.setCoupon)
          .build
      )
    }

    def createWithSubscription(
        email: String,
        stripeToken: String,
        plan: String,
        quantity: Int,
        taxRate: BigDecimal,
        coupon: Box[Coupon] = Empty
    ): Box[Stripe.Customer] =
      for {
        customer <- Customer.create(email, stripeToken, coupon)
        _        <- Subscription.createWithTaxRate(customer, taxRate, plan, quantity)
        updated  <- Stripe.Customer.retrieve(customer.id)
      } yield updated

    def update(customerId: String, params: CustomerUpdateParams): Box[Stripe.Customer] =
      Stripe.Customer.retrieve(customerId).flatMap(_.update(params))

    def delete(customerId: String): Box[Stripe.Customer] =
      Stripe.Customer.retrieve(customerId).flatMap(_.delete())

    def createCard(customerId: String, stripeToken: String): Box[Stripe.Card] = {
      val customerRetrieveParams = CustomerRetrieveParams.builder.addExpand("sources").build
      val cardCreateParams =
        PaymentSourceCollectionCreateParams.builder.setSource(stripeToken).build

      for {
        customer  <- Stripe.Customer.retrieve(customerId, customerRetrieveParams)
        sources   <- customer.sources
        newSource <- sources.create(cardCreateParams)
        if newSource.isCard
      } yield newSource.asCard
    }

    def deleteDiscount(customerId: String): Box[Stripe.Discount] =
      Stripe.Customer.retrieve(customerId).flatMap(_.deleteDiscount())
  }

  object Subscription {
    def create(
        customer: Stripe.Customer,
        taxRate: Stripe.TaxRate,
        plan: String,
        quantity: Int,
        coupon: Box[String] = Empty
    ): Box[Stripe.Subscription] =
      Stripe.Subscription.create(
        SubscriptionCreateParams.builder
          .setCustomer(customer.id)
          .whenDefined(coupon)(_.setCoupon)
          .addDefaultTaxRate(taxRate.id)
          .addItem(
            SubscriptionCreateParams.Item.builder
              .setPlan(plan) // TODO: probably should migrate to prices
              .setQuantity(quantity)
              .build
          )
          .build
      )

    def createWithTaxRate(
        customer: Stripe.Customer,
        taxRate: BigDecimal,
        plan: String,
        quantity: Int,
        coupon: Box[String] = Empty
    ): Box[Stripe.Subscription] =
      for {
        txr <- TaxRate.retrieveOrCreate(taxRate)
        sub <- Subscription.create(customer, txr, plan, quantity, coupon)
      } yield sub

    def update(subscriptionId: String, params: SubscriptionUpdateParams): Box[Stripe.Subscription] =
      Stripe.Subscription.retrieve(subscriptionId).flatMap(_.update(params))

    def replaceTaxRate(
        subscriptionId: String,
        taxRate: BigDecimal
    ): Box[Stripe.Subscription] =
      for {
        txr <- TaxRate.retrieveOrCreate(taxRate)
        sub <- Subscription.update(
                subscriptionId,
                SubscriptionUpdateParams.builder
                  .addDefaultTaxRate(txr.id)
                  .build
              )
      } yield sub

    def updateFirstItem(
        subscriptionId: String,
        params: SubscriptionItemUpdateParams
    ): Box[Stripe.Subscription] =
      for {
        subscription <- Stripe.Subscription.retrieve(subscriptionId)
        items        <- subscription.items
        first        <- items.data.headOption
        _            <- first.update(params)
        updated      <- Stripe.Subscription.retrieve(subscriptionId)
      } yield updated
  }

  object TaxRate {
    def retrieveOrCreate(taxRate: BigDecimal): Box[Stripe.TaxRate] = {
      val maybeTaxRate: Box[Stripe.TaxRate] =
        Stripe.TaxRate
          .list(
            TaxRateListParams.builder
              .setActive(true)
              .setLimit(100L)
              .build
          )
          .flatMap(_.data.find(_.percentage == taxRate))

      def createTaxRate: Box[Stripe.TaxRate] =
        Stripe.TaxRate.create(
          TaxRateCreateParams.builder
            .setDisplayName("Tax")
            .setPercentage(taxRate.bigDecimal)
            .setInclusive(false)
            .build
        )

      maybeTaxRate or createTaxRate
    }
  }

}
