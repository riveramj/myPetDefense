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
import com.mypetdefense.service._
import com.mypetdefense.actor._

import me.frmr.stripe
import org.joda.time._
import scala.language.postfixOps
import java.text.SimpleDateFormat
import java.util.Date

import dispatch.{Req => DispatchReq, _} , Defaults._

import scala.util.{Failure => TryFail, Success => TrySuccess, _}

object StripeHook extends StripeHook {
  override val emailActor = EmailActor
}

trait StripeHook extends RestHelper with Loggable {
  def emailActor: EmailActor

  def sameDateComparison(date1: Date, date2: Date) = {
    val dateFormat = new SimpleDateFormat("MM/dd/yyyy")
    
    dateFormat.format(date1) == dateFormat.format(date2)
  }

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
        if (activePets_?) {
          TaxJarService.processTaxesCharged(
            invoicePaymentId,
            city,
            state,
            zip,
            formatAmount(subtotal),
            formatAmount(tax)
          )

          ParentService.updatePuppyProducts(user)

          for {
            subscription <- user.getSubscription
            shipmentCount = subscription.shipments.toList.size
          } yield {

            val agency = user.referer.obj
            val agencyName = agency.map(_.name.get).openOr("")

            val insert = {
              (shipmentCount, agencyName) match {
                case (0, "TPP") => "TPP+Welcome Insert"
                case (0, _) => "Welcome Insert"
                case (_, _) => "-"
              }
            }

            val shipment = Shipment.createShipment(
              user,
              subscription,
              invoicePaymentId,
              formatAmount(amountPaid),
              formatAmount(tax),
              insert
            )

            val shipStationOrder = ShipStationService.createShipStationOrder(shipment, user)

            shipStationOrder.onComplete {
              case TrySuccess(Full(order)) =>
                if (!sameDateComparison(
                    new Date(),
                    shipment.expectedShipDate.get
                )) {
                  ShipStationService.holdOrderUntil(
                    order.orderId,
                    shipment.expectedShipDate.get
                  ).onComplete {
                    case TrySuccess(Full(_)) =>

                    case TrySuccess(shipStationFailure) =>
                      logger.error(s"hold order failed with shipStation error: ${shipStationFailure}")

                    case TryFail(throwable: Throwable) =>
                      logger.error(s"hold order failed with other error: ${throwable}")
                  }
                }

              case TrySuccess(shipStationFailure) =>
                logger.error(s"create order failed with shipStation error: ${shipStationFailure}")
                shipStationFailure

              case TryFail(throwable: Throwable) =>
                logger.error(s"create order failed with other error: ${throwable}")
                Empty
            }
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
