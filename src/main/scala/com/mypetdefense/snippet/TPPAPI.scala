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
    import JsonDSL._

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

  def setupStripeSubscription(parent: User, stripeToken: String, pets: List[Pet], address: NewAddress) = {
    val products = pets.flatMap(_.product.obj)
    val rawPennyCount: Double = products.map { product => 
      Price.getPricesByCode(product, Price.currentTppPriceCode).map(_.price.get) 
    }.flatten.foldLeft(0D)(_+_)

    val pennyCount = tryo((rawPennyCount * 100).toInt).openOr(0)

    val couponName = {
      if (pets.size > 1)
        Some(s"tpp-${pets.size}pets")
      else
        None
    }

    val discountAmount = (pets.size - 1) * 100

    val subtotalWithDiscount = (pennyCount - discountAmount) / 100

    if (pennyCount == 0) {
      logger.error("Penny count is 0. This seems wrong.")
      // TODO: Send price error email.
    }

    val (taxDue, taxRate) = {
      if (address.state.toLowerCase() == "ga") {
        TaxJarService.findTaxAmoutAndRate(
          address.city,
          address.state,
          address.zip,
          subtotalWithDiscount
        )
      } else {
        (0D, 0D)
      }
    }

    val stripeCustomer: Future[Box[Customer]] = Customer.create(
      email = Some(parent.email.get),
      card = Some("tok_visa"),
      plan = Some("tpp-pennyPlan"),
      quantity = Some(pennyCount),
      coupon = couponName,
      taxPercent = Some(taxRate)
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
        }
        val coupon = Coupon.find(By(Coupon.couponCode, couponName.getOrElse("")))
        val updatedUser = updatedParent.map(_.coupon(coupon).saveMe)

        updatedUser.map { user =>
          if (Props.mode == Props.RunModes.Production) {
            EmailActor ! NewSaleEmail(user, pets.size, "TPP")
          }

          EmailActor ! SendNewUserEmail(user)
        }

      case TrySuccess(stripeFailure) =>
        logger.error("create customer failed with: " + stripeFailure)
        // TODO: Send email with failure.
        stripeFailure
      case TryFail(throwable: Throwable) =>
        logger.error("create customer failed with: " + throwable)
        // TODO: Send email with failure.
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

    if (product == Empty) {
      logger.error("Didnt match product. Need manual resolution")
      // TODO: Send email with product error.
    }

      product.map(Pet.createNewPet(parent, pet.name, _))
    }).filter(_ != Empty)
  }

  serve {
    case req @ Req("api" :: "v1" :: "customer" :: Nil, _, PostRequest) =>
    {
      for {
        requestBody <- (req.body ?~ "No request body." ~> 400)
        requestJson <- tryo(Serialization.read[JValue](new String(requestBody))) ?~! "Invalid JSON." ~> 400
        parentJson <- Full(requestJson \ "parent")
        possibleParent <- tryo(parentJson.extract[NewParent]) ?~ "Error in customer json." ~> 400
        petsJson <- Full(requestJson \ "pets")
        pets <- tryo(petsJson.extract[List[NewPet]]) ?~ "Error in pets json." ~> 400
        phoneAgentEmail <- tryo(requestJson \ "phoneAgentEmail").map(_.extract[String]) ?~ "Phone agent is missing." ~> 400
        } yield {
          val salesAgent= User.find(By(User.email, phoneAgentEmail), By(User.userType, UserType.Agent))

          val salesAgency = salesAgent.flatMap(_.agency.obj)
          val existingUser = User.find(By(User.email, possibleParent.email), By(User.userType, UserType.Parent))

          existingUser match {
            case Empty => {
              val currentParent = User.createNewPendingUser(
                possibleParent,
                salesAgency,
                salesAgent
              )

              val parentAddress = Address.createNewAddress(possibleParent.address, Full(currentParent))

              val createdPets = createPets(pets, currentParent) 

              if (createdPets.size != pets.size) {
                logger.error("a pet creation failed")
                // TODO: Send email with pet error.
              }

              setupStripeSubscription(currentParent, possibleParent.stripeToken, createdPets.flatten, possibleParent.address)

              JsonResponse(
                ("id" -> s"${currentParent.userId}") ~ ("type" -> "User"),
                Nil,
                Nil,
                201
              )
            }

            case other => {
              logger.error("Conflict on user createation. Email sent.")
              // TODO: Send email with user error.

              JsonResponse(
                ("message" -> "possible duplicate parent found. data logged"),
                Nil,
                Nil,
                200
              )
            }
          }
        }
    }
  }
}
