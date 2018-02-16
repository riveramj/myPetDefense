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
import com.mypetdefense.actor._

import scala.util.{Failure => TryFail, Success => TrySuccess, _}
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

import java.util.Date

import me.frmr.stripe.{StripeExecutor, Customer, Coupon => StripeCoupon}
import dispatch.{Req => DispatchReq, _}, Defaults._

case class NewAddress(street1: String, street2: Option[String], city: String, state: String, zip: String)
case class NewPet(name: String, dateOfBirth: Option[String], product: String, currentSize: String, adultSize: String)
case class NewParent(firstName: String, lastName: String, email: String, address: NewAddress, phone: Option[String], stripeToken: String)

object TPPApi extends RestHelper with Loggable {
  val stripeSecretKey = Props.get("secret.key") openOr ""
  implicit val e = new StripeExecutor(stripeSecretKey)

  def setupStripeSubscription(parent: User, stripeToken: String, price: Int, petCount: Int) = {
    val stripeCustomer: Future[Box[Customer]] = Customer.create(
      email = Some(parent.email.get),
      card = None,
      plan = Some("pennyProduct"),
      quantity = Some(price),
      taxPercent = None,
      coupon = Some("tpp")
    )

    stripeCustomer onComplete {
      case TrySuccess(Full(customer)) =>
        val refreshedParent = parent.refresh
        val updatedParent = refreshedParent.map(_.stripeId(customer.id).saveMe)

        val subscriptionId = (
          for {
            rawSubscriptions <- customer.subscriptions
            subscription <- rawSubscriptions.data.headOption
            } yield {
              subscription.id
            }).flatMap(identity).getOrElse("")

        updatedParent.map { user =>
          Subscription.createNewSubscription(
            user,
            subscriptionId,
            new Date(),
            new Date(),
            Price.currentTppPriceCode
          )

          if (Props.mode == Props.RunModes.Production) {
            EmailActor ! NewSaleEmail(user, petCount, "TPP")
          }

          EmailActor ! SendNewUserEmail(user)
        }

      case TrySuccess(stripeFailure) =>
        logger.error("create customer failed with: " + stripeFailure)
        stripeFailure
      case TryFail(throwable: Throwable) =>
        logger.error("create customer failed with: " + throwable)
        throwable
    }
  }

  def createPets(pets: List[NewPet], parent: User) = {
    (pets.map { pet =>
      val sanitizedProductName = pet.product.toLowerCase match {
        case possibleProduct 
            if possibleProduct.contains("zoguard") && possibleProduct.toLowerCase.contains("dog") =>
          "ZoGuard Plus for Dogs"
        
        case possibleProduct 
            if possibleProduct.contains("zoguard") && possibleProduct.toLowerCase.contains("cat") =>
          "ZoGuard Plus for Cats"
        
        case possibleProduct => 
          possibleProduct
      }

      val sanitizedSize = pet.currentSize.toLowerCase match {
        case "small" => "Small"
        case "medium" => "Medium"
        case "large" => "Large"
        case "xlarge" => "XLarge"
        case size => size
      }

      val product = Product.find(
        By(Product.name, sanitizedProductName),
        By(Product.sizeName, sanitizedSize)
      )

      product.map(Pet.createNewPet(parent, pet.name, _))
    }).filter(_ != Empty)
  }

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
          val pennyCount = pets.size * 1299

          val newParent = User.createNewPendingUser(
            possibleParent,
            salesAgency,
            salesAgent
          )

          val parentAddress = Address.createNewAddress(possibleParent.address, Full(newParent))

          val createdPets = createPets(pets, newParent) 

          if (createdPets.size != pets.size) {
            logger.error("a pet creation failed")
          }

          setupStripeSubscription(newParent, possibleParent.stripeToken, pennyCount, pets.size)

          JsonResponse(requestJson, Nil, Nil, 201)
        }
    }
  }
}
