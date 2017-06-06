package com.mypetdefense.service

import net.liftweb._ 
  import common._
  import util.Helpers.tryo
  import json._
  import util.Props

import me.frmr.stripe.{StripeExecutor, Plan}
import scala.util.{Failure => TryFail, Success => TrySuccess, _}

import com.mypetdefense.model._

import scala.concurrent.Await
import scala.concurrent.duration._

import dispatch._, Defaults._

object PriceService extends Loggable {
  val stripeSecretKey = Props.get("secret.key") openOr ""
  implicit val e = new StripeExecutor(stripeSecretKey)

  def createStripePrice(price: Double, priceId: Long, name: String) = {
    val stripeAmount: Long = (price * 100).toLong

    val possiblePlan = Plan.create(
      id = priceId.toString(),
      amount = stripeAmount,
      name = name,
      currency = "USD",
      interval = "month"
    )

    Try(Await.result(possiblePlan, new DurationInt(3).seconds)) match {
      case TrySuccess(Full(newPlan)) =>
        Full(newPlan)

      case TrySuccess(stripeFailure) =>
        logger.error("create coupon failed with stipe error: " + stripeFailure)
        Empty

      case TryFail(throwable: Throwable) =>
        logger.error("create coupon failed with other error: " + throwable)
        Empty
    }
  }
}

