package com.mypetdefense.service

import com.mypetdefense.model.Coupon
import com.mypetdefense.service.{StripeBoxAdapter => Stripe}
import com.mypetdefense.util.StripeHelper._
import com.stripe.param.SubscriptionUpdateParams.ProrationBehavior
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
        items: List[Subscription.Item]
    ): Box[CustomerWithSubscriptions] =
      for {
        customer <- Customer.create(email, stripeToken, coupon)
        _        <- Subscription.createWithTaxRate(customer, taxRate, Empty, items)
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
        items: List[Item]
    ): Box[Stripe.Subscription] = {
      val stripeItems =
        items.map {
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
          .addAllItem(stripeItems)
          .build
      )
    }

    def createWithTaxRate(
        customer: Stripe.Customer,
        taxRate: BigDecimal,
        coupon: Box[String],
        items: List[Item]
    ): Box[Stripe.Subscription] =
      for {
        txr <- TaxRate.retrieveOrCreate(taxRate)
        sub <- Subscription.create(customer, txr, coupon, items)
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

    def updateSubscriptionItem(subscriptionId: String, pets: Map[String, Int]): Box[Stripe.Subscription] = {
      val changePrice = com.mypetdefense.model.Price.getChangeProduct()
      val stripePriceId = changePrice.map(_.stripePriceId.get).openOr("")
      val stripePriceItem = SubscriptionUpdateParams.Item.builder().setPrice(stripePriceId).setQuantity(1).build()

      val addChangeProduct = SubscriptionUpdateParams
        .builder()
        .addItem(stripePriceItem)
        .setProrationBehavior(SubscriptionUpdateParams.ProrationBehavior.NONE)
        .build()

      val updateParams = for {
        (priceId, count) <- pets
      } yield {
        SubscriptionUpdateParams.Item.builder()
          .setPrice(priceId)
          .setQuantity(count)
          .build()
      }

      (for {
        subscription <- Stripe.Subscription.retrieve(subscriptionId).toList
        _ = println(subscriptionId)
          if !subscription.status.contains("incomplete_expired")
        updatedSubscription <- subscription.update(addChangeProduct).toList
        items <- updatedSubscription.items.toList
      } yield {
        val (currentItems, changeItem) = items.data.partition { item =>
          item.underlying.getPrice.getId != stripePriceId
        }

        val updateItems = SubscriptionItemDeleteParams.builder()
          .setProrationBehavior(SubscriptionItemDeleteParams.ProrationBehavior.NONE)
          .build()

        currentItems.map(_.underlying.delete(updateItems))

        val updatedSubscription = subscription.update(
          SubscriptionUpdateParams.builder()
            .addAllItem(updateParams.toList.asJava)
            .setProrationBehavior(ProrationBehavior.NONE)
            .build()
        )

        changeItem.map(_.underlying.delete(updateItems))

        updatedSubscription
      }).flatten.headOption
    }

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

  object Product {
    def create(name: String): Box[StripeBoxAdapter.Product] ={
      Stripe.Product.create(
        ProductCreateParams.builder()
          .setName(name)
          .build
      )
    }
  }

  object Price {
    def create(
      productId: String,
      cost: Long,
      name: String
    ): Box[StripeBoxAdapter.Price] = {
      val recurringDetails = PriceCreateParams.Recurring.builder()
        .setInterval(PriceCreateParams.Recurring.Interval.MONTH)
        .build

      Stripe.Price.create(
        PriceCreateParams.builder()
          .setProduct(productId)
          .setCurrency("usd")
          .setUnitAmount(cost)
          .setRecurring(recurringDetails)
          .setNickname(name)
          .build
      )
    }
  }

}
