package com.mypetdefense.service

import com.mypetdefense.model.Coupon
import com.mypetdefense.service.{StripeBoxAdapter => Stripe}
import com.mypetdefense.util.StripeHelper._
import com.stripe.param._
import net.liftweb.common.{Box, Empty}

import scala.collection.JavaConverters._

object StripeFacade {

  final case class CustomerWithSources(value: Stripe.Customer)       extends AnyVal
  final case class CustomerWithSubscriptions(value: Stripe.Customer) extends AnyVal

  object Coupon {
    def delete(couponId: String): Box[Stripe.Coupon] =
      Stripe.Coupon.retrieve(couponId).flatMap(_.delete())
  }

  object Customer {
    def retrieveWithSources(customerId: String): Box[CustomerWithSources] = {
      val params = CustomerRetrieveParams.builder.addExpand("sources").build
      Stripe.Customer.retrieve(customerId, params).map(CustomerWithSources)
    }

    def retrieveWithSubscriptions(customerId: String): Box[CustomerWithSubscriptions] = {
      val params = CustomerRetrieveParams.builder.addExpand("subscriptions").build
      Stripe.Customer.retrieve(customerId, params).map(CustomerWithSubscriptions)
    }

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
        taxRate: BigDecimal,
        coupon: Box[Coupon],
        firstItem: Subscription.Item,
        otherItems: Subscription.Item*
    ): Box[CustomerWithSubscriptions] =
      for {
        customer <- Customer.create(email, stripeToken, coupon)
        _        <- Subscription.createWithTaxRate(customer, taxRate, Empty, firstItem, otherItems: _*)
        updated  <- Customer.retrieveWithSubscriptions(customer.id)
      } yield updated

    def update(customerId: String, params: CustomerUpdateParams): Box[Stripe.Customer] =
      Stripe.Customer.retrieve(customerId).flatMap(_.update(params))

    def delete(customerId: String): Box[Stripe.Customer] =
      Stripe.Customer.retrieve(customerId).flatMap(_.delete())

    def createCard(customerId: String, stripeToken: String): Box[Stripe.Card] = {
      val cardCreateParams =
        PaymentSourceCollectionCreateParams.builder.setSource(stripeToken).build

      for {
        customer  <- Customer.retrieveWithSources(customerId)
        sources   <- customer.value.sources
        newSource <- sources.create(cardCreateParams)
        if newSource.isCard
      } yield newSource.asCard
    }

    def deleteDiscount(customerId: String): Box[Stripe.Discount] =
      Stripe.Customer.retrieve(customerId).flatMap(_.deleteDiscount())
  }

  object Subscription {
    final case class Item(priceId: String, quantity: Int = 1)

    def create(
        customer: Stripe.Customer,
        taxRate: Stripe.TaxRate,
        coupon: Box[String],
        firstItem: Item,
        otherItems: Item*
    ): Box[Stripe.Subscription] = {
      val items =
        (firstItem +: otherItems).map {
          case Item(priceId, quantity) =>
            SubscriptionCreateParams.Item.builder
              .setPrice(priceId)
              .setQuantity(quantity)
              .build
        }.asJava

      Stripe.Subscription.create(
        SubscriptionCreateParams.builder
          .setCustomer(customer.id)
          .whenDefined(coupon)(_.setCoupon)
          .addDefaultTaxRate(taxRate.id)
          .addAllItem(items)
          .build
      )
    }

    def createWithTaxRate(
        customer: Stripe.Customer,
        taxRate: BigDecimal,
        coupon: Box[String],
        firstItem: Item,
        otherItems: Item*
    ): Box[Stripe.Subscription] =
      for {
        txr <- TaxRate.retrieveOrCreate(taxRate)
        sub <- Subscription.create(customer, txr, coupon, firstItem, otherItems: _*)
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
        subscription: Stripe.Subscription,
        params: SubscriptionItemUpdateParams
    ): Box[Stripe.Subscription] =
      for {
        items   <- subscription.items
        first   <- items.data.headOption
        _       <- first.update(params)
        updated <- Stripe.Subscription.retrieve(subscription.id)
      } yield updated

    def updateFirstItem(
        subscriptionId: String,
        params: SubscriptionItemUpdateParams
    ): Box[Stripe.Subscription] =
      for {
        subscription <- Stripe.Subscription.retrieve(subscriptionId)
        updated      <- Subscription.updateFirstItem(subscription, params)
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
