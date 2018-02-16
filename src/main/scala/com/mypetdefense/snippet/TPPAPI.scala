package com.mypetdefense.snippet 

import net.liftweb._
  import common._
  import mapper._
  import http._
    import LiftRules._
    import rest._
    import js._
      import JE._
      import JsExp._
  import util._
    import Helpers._
  import json._
    import Extraction._

import com.mypetdefense.model._
import com.mypetdefense.service.{TaxJarService, ParentService}

import scala.util.{Failure => TryFail, Success => TrySuccess, _}
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

import me.frmr.stripe.{StripeExecutor, Customer, Coupon => StripeCoupon}
import dispatch.{Req => DispatchReq, _}, Defaults._

case class NewAddress(street1: String, street2: Option[String], city: String, state: String, zip: String)
case class NewPet(name: String, dateOfBirth: Option[String], product: String, currentSize: Option[String], adultSize: String)
case class NewParent(firstName: String, lastName: String, email: String, address: NewAddress, phone: Option[String], stripeToken: String)

object TPPApi extends RestHelper with Loggable {
  val stripeSecretKey = Props.get("secret.key") openOr ""
  implicit val e = new StripeExecutor(stripeSecretKey)

  serve {
    case req @ Req("api" :: "v1" :: "customer" :: Nil, _, PostRequest) =>
      {
        for {
          requestBody <- (req.body ?~ "No request body." ~> 400)
          requestJson <- tryo(Serialization.read[JValue](new String(requestBody))) ?~! "Invalid JSON." ~> 400
          phoneAgentEmail <- tryo(requestJson \ "phoneAgentEmail").map(_.extract[String]) ?~ "Phone agent is missing." ~> 400
          parentJson <- Full(requestJson \ "parent")
          possibleParent <- tryo(parentJson.extract[NewParent]) ?~ "Error in customer json." ~> 400
          petsJson <- Full(requestJson \ "pets")
          pets <- tryo(petsJson.extract[List[NewPet]]) ?~ "Error in pets json." ~> 400
        } yield {
          val salesAgent= User.find(By(User.email, phoneAgentEmail), By(User.userType, UserType.Agent))

          val salesAgency = salesAgent.flatMap(_.agency.obj)

          val newParent = User.createNewPendingUser(
            possibleParent,
            salesAgency,
            salesAgent
          )

          val parentAddress = Address.createNewAddress(possibleParent.address, Full(newParent))

          val pennyCount = pets.size * 1299
          
          val stripeCustomer: Future[Box[Customer]] = Future {
            val possibleStripeCustomer = Customer.create(
              email = Some(possibleParent.email),
              card = None,
              plan = Some("pennyProduct"),
              quantity = Some(pennyCount),
              taxPercent = None,
              coupon = Some("tpp")
            )

            Try(Await.result(possibleStripeCustomer, new DurationInt(7).seconds)) match {
              case TrySuccess(Full(customer)) =>
                println("in success")
                Full(customer)
              case TrySuccess(stripeFailure) =>
                logger.error("create customer failed with: " + stripeFailure)
                stripeFailure
              case TryFail(throwable: Throwable) =>
                logger.error("create customer failed with: " + throwable)
                Empty
            }
          }


          stripeCustomer onComplete {
            case TrySuccess(Full(customer)) =>
              val updatedParent = newParent.refresh
              val updated = updatedParent.map(_.stripeId(customer.id).saveMe)
              println(updated)
            case TryFail(t) => println("An error has occured: " + t.getMessage)
          }

          JsonResponse(requestJson, Nil, Nil, 201)
        }
      }
  }
}
