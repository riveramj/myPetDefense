package com.mypetdefense.util

import com.mypetdefense.typeclasses.MaybeLike
import com.mypetdefense.typeclasses.MaybeLike.Syntax
import com.stripe.model._
import com.stripe.net.ApiResource
import com.stripe.param._
import net.liftweb.common.Box

import scala.collection.JavaConverters._
import scala.language.higherKinds

object StripeHelper {

  implicit class ApiRequestParamsBuilderOps[B](val builder: B) extends AnyVal {
    def whenDefined[F[_]: MaybeLike, T](maybeValue: F[T])(setValue: B => T => B): B = {
      maybeValue foreach setValue(builder)
      builder
    }

    def whenDefinedC[F[_]: MaybeLike, T, R](
        maybeValue: F[T]
    )(setValue: B => R => B)(implicit ev: T => R): B =
      whenDefined(maybeValue.map(ev))(setValue)

    def when(cond: Boolean)(thenPart: B => Unit)(elsePart: B => Unit): B = {
      if (cond) thenPart(builder) else elsePart(builder)
      builder
    }
  }

  class BoxedApiRequest[R] private (private val resource: Box[R]) {
    def toBox: Box[R] = resource

    def andThenBoxRight[R1 <: ApiResource](f: R => Box[R1]): BoxedApiRequest[R1] =
      new BoxedApiRequest(resource.flatMap(f))

    def andThenBoxLeft[R1 <: ApiResource](f: R => Box[R1]): BoxedApiRequest[R] =
      new BoxedApiRequest(resource.flatMap(f).flatMap(_ => resource))

    def andThenRight[R1 <: ApiResource](f: R => R1): BoxedApiRequest[R1] =
      andThenBoxRight(safeBox(f))

    def andThenLeft[R1 <: ApiResource](f: R => R1): BoxedApiRequest[R] =
      andThenBoxLeft(safeBox(f))

    def zipBox[R1 <: ApiResource](box: => Box[R1]): BoxedApiRequest[(R, R1)] =
      new BoxedApiRequest(resource.flatMap(l => box.map(r => (l, r))))

    def map[R1](f: R => R1): BoxedApiRequest[R1] =
      new BoxedApiRequest(resource.map(f))

    private def safeBox[R1 <: ApiResource](f: R => R1): R => Box[R1] =
      res => Box.tryo(f(res))
  }

  object BoxedApiRequest {
    def apply[R <: ApiResource](resource: => R): BoxedApiRequest[R] =
      new BoxedApiRequest(Box.tryo(resource))
  }

  def deleteCoupon(couponId: String): Box[Coupon] =
    doOnApiResource { Coupon.retrieve(couponId) } { _.delete() }

  def updateCustomer(customerId: String, params: CustomerUpdateParams): Box[Customer] =
    doOnApiResource { Customer.retrieve(customerId) } { _.update(params) }

  def deleteCustomer(customerId: String): Box[Customer] =
    doOnApiResource { Customer.retrieve(customerId) } { _.delete() }

  def createCustomerCard(customerId: String, stripeToken: String): Box[Card] = {
    val rawSource = doOnApiResource {
      val params = CustomerRetrieveParams.builder.addExpand("sources").build
      Customer.retrieve(customerId, params, null)
    } { customer =>
      val params = PaymentSourceCollectionCreateParams.builder.setSource(stripeToken).build
      customer.getSources.create(params)
    }

    rawSource
      .filter(_.isInstanceOf[Card])
      .map(_.asInstanceOf[Card])
  }

  def deleteCustomerDiscount(customerId: String): Box[Unit] =
    doOnApiResource { Customer.retrieve(customerId) } { _.deleteDiscount() }

  def updateSubscription(
      subscriptionId: String,
      params: SubscriptionUpdateParams
  ): Box[Subscription] =
    doOnApiResource { Subscription.retrieve(subscriptionId) } { _.update(params) }

  def updateFirstSubscriptionItem(
      subscriptionId: String,
      params: SubscriptionItemUpdateParams
  ): Box[SubscriptionItem] =
    doOnApiResource { Subscription.retrieve(subscriptionId) } {
      _.getItems.getData.asScala.headOption.map(_.update(params))
    }.flatMap(Box(_))

  @inline private def doOnApiResource[R, T](retrieveRes: => R)(doOnRes: R => T): Box[T] =
    for {
      resource <- Box.tryo(retrieveRes)
      result   <- Box.tryo(doOnRes(resource))
    } yield result

}
