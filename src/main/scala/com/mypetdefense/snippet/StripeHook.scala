import com.mypetdefense.snippet

import net.liftweb._
  import common._
  import http._
    import LiftRules._
    import rest._
  import util._
    import Helpers._
  import json._
    import Extraction._

import me.frmr.stripe

object StripeHook extends StripeHook

trait StripeHook extends RestHelper with Loggable {

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
            case "invoice.payment_succeeded" => println("payment succeeded"); Empty
            case "invoice.payment_failed" => println("payment failed"); Empty
            case "customer.subscription.created" => println("subscription created"); Empty
            case "customer.subscription.updated" => println("subscription updated"); Empty
            case "customer.subscription.deleted" => println("subscription deleted"); Empty
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
