package com.mypetdefense.util

import java.util.{Map => JMap}

import com.stripe.model._
import net.liftweb.common.Box

import scala.collection.JavaConverters._

object StripeHelper {

  type Param = (String, AnyRef)

  type ParamsMap = JMap[String, AnyRef]
  def ParamsMap(kvs: Option[Param]*): ParamsMap = kvs.flatten.toMap.asJava

  implicit class StripeParamOps(key: String) {
    def -->(value: Any): Option[Param] =
      Some((key, value.toString))

    def -?>(value: Option[Any]): Option[Param] =
      value.map(v => (key, v.toString))
  }

  def deleteCoupon(couponId: String): Box[DeletedCoupon] =
    doOnApiResource { Coupon.retrieve(couponId) } { _.delete() }

  def updateCustomer(customerId: String, params: ParamsMap): Box[Customer] =
    doOnApiResource { Customer.retrieve(customerId) } { _.update(params) }

  def deleteCustomer(customerId: String): Box[DeletedCustomer] =
    doOnApiResource { Customer.retrieve(customerId) } { _.delete() }

  def createCustomerCard(customerId: String, stripeToken: String): Box[Card] = {
    val rawSource = doOnApiResource {
      Customer.retrieve(customerId, ParamsMap("expand" --> List("sources").asJava), null)
    } { _.getSources.create(ParamsMap("source" --> stripeToken)) }

    rawSource
      .filter(_.getObject == "card")
      .map(_.asInstanceOf[Card])
  }

  def deleteCustomerDiscount(customerId: String): Box[Unit] =
    doOnApiResource { Customer.retrieve(customerId) } { _.deleteDiscount() }

  def updateSubscription(subscriptionId: String, params: ParamsMap): Box[Subscription] =
    doOnApiResource { Subscription.retrieve(subscriptionId) } { _.update(params) }

  @inline private def doOnApiResource[R, T](retrieveRes: => R)(doOnRes: R => T): Box[T] =
    for {
      resource <- Box.tryo(retrieveRes)
      result   <- Box.tryo(doOnRes(resource))
    } yield result

}
