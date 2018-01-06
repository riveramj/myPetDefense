package com.mypetdefense.snippet

import net.liftweb._
  import mapper.By
  import common._
  import http._
    import LiftRules._
    import rest._
  import util._
    import Helpers._
  import json._
    import Extraction._

import com.mypetdefense.model._
import com.mypetdefense.service.{TaxJarService, ParentService}
import com.mypetdefense.actor._

import me.frmr.stripe
import org.joda.time._
import scala.language.postfixOps

object StripeHook extends StripeHook {
  override val emailActor = EmailActor
}

trait StripeHook extends RestHelper with Loggable {
  def emailActor: EmailActor

  def invoicePaymentSucceeded(objectJson: JValue) = {
    for {
      stripeCustomerId <- tryo((objectJson \ "customer").extract[String]) ?~! "No customer."
      stripeSubscriptionId <- tryo((objectJson \ "subscription").extract[String]) ?~! "No subscription id."
      subtotal <- tryo((objectJson \ "subtotal").extract[String]) ?~! "No subtotal"
      tax <- tryo((objectJson \ "tax").extract[String]) ?~! "No tax paid"
      amountPaid <- tryo((objectJson \ "amount_due").extract[String]) ?~! "No amount paid"
      user <- User.find(By(User.stripeId, stripeCustomerId))
      shippingAddress <- Address.find(By(Address.user, user), By(Address.addressType, AddressType.Shipping))
      invoicePaymentId <- tryo((objectJson \ "id").extract[String]) ?~! "No ID."
    } yield {

      val notTrial_? = ParentService.notTrialSubscription_?(stripeCustomerId, stripeSubscriptionId)
      val city = shippingAddress.city.get
      val state = shippingAddress.state.get
      val zip = shippingAddress.zip.get
      val activePets_? = user.activePets.length > 0
      
      def formatAmount(possibleAmount: String) = {
        val formattedAmount = tryo(possibleAmount.toDouble/100.0).openOr(0D)

        if (formattedAmount == 0D)
          0.toString
        else
          f"$formattedAmount%2.2f"
      }

      if (notTrial_? && activePets_?) {
        TaxJarService.processTaxesCharged(
          invoicePaymentId,
          city,
          state,
          zip,
          formatAmount(subtotal),
          formatAmount(tax)
        )

        if (activePets_?) {
          val shipment = Shipment.createShipment(
            user,
            invoicePaymentId,
            formatAmount(amountPaid),
            formatAmount(tax)
          )

          shipment.map( ship => ShipmentLineItem.find(By(ShipmentLineItem.shipment, ship)))

          if (Props.mode != Props.RunModes.Pilot) {
            emailActor ! PaymentReceivedEmail(user, tryo(amountPaid.toDouble/100.0).openOr(0D))
          }
        }
      }

      OkResponse()
    }
  }

  def invoicePaymentFailed(objectJson: JValue) = {
    for {
      stripeCustomerId <- tryo((objectJson \ "customer").extract[String]) ?~! "No customer."
      user <- User.find(By(User.stripeId, stripeCustomerId))
      totalAmountInCents <- tryo((objectJson \ "total").extract[Long]) ?~! "No total."
      } yield {
        val nextPaymentAttemptSecs: Option[Long] =
          tryo((objectJson \ "next_payment_attempt").extract[Option[Long]]).openOr(None)

        val nextPaymentAttempt = nextPaymentAttemptSecs.map { nextPaymentAttemptInSecs =>
          new DateTime(nextPaymentAttemptInSecs * 1000)
          } filter {
            _ isAfterNow
          }
          val amount = totalAmountInCents / 100d

          emailActor ! SendInvoicePaymentFailedEmail(user, amount, nextPaymentAttempt)

          OkResponse()
      }
  }

  def subscriptionPastDue(objectJson: JValue) = {
    for {
      stripeCustomerId <- tryo((objectJson \ "customer").extract[String]) ?~! "No customer."
      user <- User.find(By(User.stripeId, stripeCustomerId))
      accountStatus <- tryo((objectJson \ "status").extract[String]) ?~! "No status."
      } yield {
        if (accountStatus == "past_due")
          user.subscription.map(_.status(Status.BillingSuspended).saveMe)

        OkResponse()
      }
  }

  serve {
    case req @ Req("stripe-hook" :: Nil, _, PostRequest) =>
      {
        for {
          requestBody <- req.body
          requestJson <- tryo(Serialization.read[JValue](new String(requestBody)))
          id <- (requestJson \ "id").extractOpt[String]
          eventType <- (requestJson \ "type").extractOpt[String]
          dataJson = (requestJson \ "data")
          objectJson = (dataJson \ "object")
        } yield {
          val result: Box[LiftResponse] = eventType match {
            case "invoice.payment_succeeded" => invoicePaymentSucceeded(objectJson)
            case "invoice.payment_failed" => invoicePaymentFailed(objectJson)
            case "customer.subscription.updated" => subscriptionPastDue(objectJson)
            case _ => Full(OkResponse())
          }

          result match {
            case Full(resp) if resp.isInstanceOf[OkResponse] => resp
            case Full(resp) => resp
            case Empty => NotFoundResponse()
            case Failure(msg, _, _) => PlainTextResponse(msg, Nil, 500)
          }
        }
      }
  }
}
