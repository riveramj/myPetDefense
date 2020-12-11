package com.mypetdefense.service

import com.mypetdefense.util.BoxHelper._
import com.stripe.param._
import com.stripe.{model => StripeModel}
import net.liftweb.common.Box

import scala.collection.JavaConverters._

object StripeBoxAdapter {

  final case class Card(underlying: StripeModel.Card) {
    def id: String    = underlying.getId
    def last4: String = underlying.getLast4
  }

  final case class Charge(underlying: StripeModel.Charge) {
    def id: String = underlying.getId
  }

  object Charge {
    def create(params: ChargeCreateParams): Box[Charge] =
      Box.tryo { StripeModel.Charge.create(params) }.map(Charge(_))
  }

  final case class Coupon(underlying: StripeModel.Coupon) {
    def amountOff: Box[Long] =
      Box.nullable(underlying.getAmountOff).map(_.toLong)
    def percentOff: Box[BigDecimal] =
      Box.nullable(underlying.getPercentOff).map(BigDecimal(_))

    def delete(): Box[Coupon] =
      Box.tryo { underlying.delete() }.map(Coupon(_))
  }

  object Coupon {
    def retrieve(coupon: String): Box[Coupon] =
      Box.tryo { StripeModel.Coupon.retrieve(coupon) }.map(Coupon(_))
    def create(params: CouponCreateParams): Box[Coupon] =
      Box.tryo { StripeModel.Coupon.create(params) }.map(Coupon(_))
  }

  final case class Customer(underlying: StripeModel.Customer) {
    def id: String = underlying.getId
    def discount: Box[Discount] =
      Box.nullable(underlying.getDiscount).map(Discount)
    def sources: Box[PaymentSourceCollection] =
      Box.nullable(underlying.getSources).map(PaymentSourceCollection)
    def subscriptions: Box[SubscriptionCollection] =
      Box.nullable(underlying.getSubscriptions).map(SubscriptionCollection)

    def update(params: CustomerUpdateParams): Box[Customer] =
      Box.tryo { underlying.update(params) }.map(Customer(_))
    def delete(): Box[Customer] =
      Box.tryo { underlying.delete() }.map(Customer(_))
    def deleteDiscount(): Box[Discount] =
      Box.tryo { underlying.deleteDiscount() }.map(Discount)
  }

  object Customer {
    def retrieve(customer: String): Box[Customer] =
      Box.tryo { StripeModel.Customer.retrieve(customer) }.map(Customer(_))
    def retrieve(customer: String, params: CustomerRetrieveParams): Box[Customer] =
      Box.tryo { StripeModel.Customer.retrieve(customer, params, null) }.map(Customer(_))
    def create(params: CustomerCreateParams): Box[Customer] =
      Box.tryo { StripeModel.Customer.create(params) }.map(Customer(_))
  }

  final case class Discount(underlying: StripeModel.Discount) {
    def coupon: Box[Coupon] =
      Box.nullable(underlying.getCoupon).map(Coupon(_))
  }

  final case class Invoice(underlying: StripeModel.Invoice) {
    def amountDue: Box[Long] =
      Box.nullable(underlying.getAmountDue).map(_.toLong)
    def created: Box[Long] =
      Box.nullable(underlying.getCreated).map(_.toLong)
    def discount: Box[Discount] =
      Box.nullable(underlying.getDiscount).map(Discount)
    def subtotal: Box[Long] =
      Box.nullable(underlying.getSubtotal).map(_.toLong)
    def tax: Box[Long] =
      Box.nullable(underlying.getTax).map(_.toLong)
  }

  object Invoice {
    def retrieve(invoice: String): Box[Invoice] =
      Box.tryo { StripeModel.Invoice.retrieve(invoice) }.map(Invoice(_))
    def upcoming(params: InvoiceUpcomingParams): Box[Invoice] =
      Box.tryo { StripeModel.Invoice.upcoming(params) }.map(Invoice(_))
  }

  final case class PaymentSource(underlying: StripeModel.PaymentSource) {
    def isCard: Boolean = underlying.isInstanceOf[StripeModel.Card]
    def asCard: Card    = Card(underlying.asInstanceOf[StripeModel.Card])
  }

  final case class PaymentSourceCollection(underlying: StripeModel.PaymentSourceCollection) {
    def data: List[PaymentSource] = underlying.getData.asScala.map(PaymentSource).toList

    def create(params: PaymentSourceCollectionCreateParams): Box[PaymentSource] =
      Box.tryo { underlying.create(params) }.map(PaymentSource)
  }

  final case class Plan(underlying: StripeModel.Plan) {
    def id: String = underlying.getId
  }

  object Plan {
    def retrieve(plan: String): Box[Plan] =
      Box.tryo { StripeModel.Plan.retrieve(plan) }.map(Plan(_))
  }

  final case class Refund(underlying: StripeModel.Refund)

  object Refund {
    def create(params: RefundCreateParams): Box[Refund] =
      Box.tryo { StripeModel.Refund.create(params) }.map(Refund(_))
  }

  final case class Subscription(underlying: StripeModel.Subscription) {
    def id: String = underlying.getId
    def currentPeriodEnd: Box[Long] =
      Box.nullable(underlying.getCurrentPeriodEnd).map(_.toLong)
    def items: Box[SubscriptionItemCollection] =
      Box.nullable(underlying.getItems).map(SubscriptionItemCollection)
    def status: Box[String] =
      Box.nullable(underlying.getStatus)

    def update(params: SubscriptionUpdateParams): Box[Subscription] =
      Box.tryo { underlying.update(params) }.map(Subscription(_))
  }

  object Subscription {
    def retrieve(subscriptionExposedId: String): Box[Subscription] =
      Box.tryo { StripeModel.Subscription.retrieve(subscriptionExposedId) }.map(Subscription(_))
    def create(params: SubscriptionCreateParams): Box[Subscription] =
      Box.tryo { StripeModel.Subscription.create(params) }.map(Subscription(_))
  }

  final case class SubscriptionCollection(underlying: StripeModel.SubscriptionCollection) {
    def data: List[Subscription] = underlying.getData.asScala.map(Subscription(_)).toList
  }

  final case class SubscriptionItem(underlying: StripeModel.SubscriptionItem) {
    def subscription: String = underlying.getSubscription

    def update(params: SubscriptionItemUpdateParams): Box[SubscriptionItem] =
      Box.tryo { underlying.update(params) }.map(SubscriptionItem)
  }

  final case class SubscriptionItemCollection(underlying: StripeModel.SubscriptionItemCollection) {
    def data: List[SubscriptionItem] = underlying.getData.asScala.map(SubscriptionItem).toList
  }

  final case class TaxRate(underlying: StripeModel.TaxRate) {
    def id: String             = underlying.getId
    def percentage: BigDecimal = BigDecimal(underlying.getPercentage)
  }

  object TaxRate {
    def list(params: TaxRateListParams): Box[TaxRateCollection] =
      Box.tryo { StripeModel.TaxRate.list(params) }.map(TaxRateCollection)
    def create(params: TaxRateCreateParams): Box[TaxRate] =
      Box.tryo { StripeModel.TaxRate.create(params) }.map(TaxRate(_))
  }

  final case class TaxRateCollection(underlying: StripeModel.TaxRateCollection) {
    def data: List[TaxRate] = underlying.getData.asScala.map(TaxRate(_)).toList
  }

}
