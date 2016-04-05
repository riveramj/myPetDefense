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

import me.frmr.stripe
import org.joda.time._
import scala.language.postfixOps

object StripeHook extends StripeHook

trait StripeHook extends RestHelper with Loggable {

  def invoicePaymentSucceeded(objectJson: JValue) = {
    for {
      stripeCustomerId <- tryo((objectJson \ "customer").extract[String]) ?~! "No customer."
      parent <- Parent.find(By(Parent.stripeId, stripeCustomerId))
      invoicePaymentId <- tryo((objectJson \ "id").extract[String]) ?~! "No ID."
    } yield {
      val shipment = Shipment.createShipment(parent, invoicePaymentId)
      
      println("===================")
      println("shipment:")
      println(shipment)

      println("===================")
      println("lines:")

      val lines = shipment.map( ship => ShipmentLineItem.find(By(ShipmentLineItem.shipment, ship)))
      println(lines)

      //emailActor ! SendInvoicePaymentSucceededEmail(user, invoice)
      OkResponse()
    }
  }

  def invoicePaymentFailed(objectJson: JValue) = {
    for {
      stripeCustomerId <- tryo((objectJson \ "customer").extract[String]) ?~! "No customer."
      parent <- Parent.find("stripeCustomerId" -> stripeCustomerId)
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

      println(s"payment failed for ${parent}")

      //emailActor ! SendInvoicePaymentFailedEmail(parent.email, amount, nextPaymentAttempt)
      
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
            case _ => Full(OkResponse())
          }

          result match {
            case Full(resp) if resp.isInstanceOf[OkResponse] =>
              resp

            case Full(resp) => resp

            case Empty => NotFoundResponse()

            case Failure(msg, _, _) =>
              PlainTextResponse(msg, Nil, 500)
          }
        }
      }
  }
}
