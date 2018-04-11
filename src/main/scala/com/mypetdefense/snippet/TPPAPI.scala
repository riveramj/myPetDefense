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
import java.text.SimpleDateFormat

import me.frmr.stripe.{StripeExecutor, Customer, Coupon => StripeCoupon, Subscription => StripeSubscription}
import dispatch.{Req => DispatchReq, _}, Defaults._

case class NewAddress(street1: String, street2: Option[String], city: String, state: String, zip: String)
case class NewPet(name: String, whelpDate: Option[String], product: String, currentSize: String, breed: String)
case class NewParent(firstName: String, lastName: String, email: String, address: NewAddress, phone: Option[String], stripeToken: String)

object TPPApi extends RestHelper with Loggable {
  val stripeSecretKey = Props.get("secret.key") openOr ""
  implicit val e = new StripeExecutor(stripeSecretKey)

  val whelpDateFormat = new java.text.SimpleDateFormat("M/d/y")

  def sendStripeErrorEmail(
    failedStepMessage: String,
    stripeFailure: Any,
    parent: Box[User],
    stripeToken: String,
    pennyCount: Int,
    couponName: Option[String],
    taxRate: Double
  ) = {
    val errorMsg = s"""
      Something went wrong with stripe creation.

      ${failedStepMessage}

      Error given was:
      ===============
      ${stripeFailure}
      ===============

      stripe Info:
      ===============
      parent:
      ${parent}

      token:
      ${stripeToken}

      plan:
      tpp-pennyPlan

      quantity:
      ${pennyCount}

      coupon:
      ${couponName}

      taxPercent:
      ${taxRate}
      ===============
    """

    EmailActor ! SendAPIErrorEmail(errorMsg)
  }

  def setupStripeSubscription(
    oldParent: User,
    stripeToken: String,
    newUser: Boolean = true
  ) = {
    val refreshedUser = oldParent.refresh
    val pets = refreshedUser.map(_.pets.toList).openOr(Nil)
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

    val address = refreshedUser.flatMap(_.addresses.toList.headOption)

    val discountAmount = (pets.size - 1) * 100

    val subtotalWithDiscount = (pennyCount - discountAmount) / 100

    if (pennyCount == 0) {
      logger.error("Penny count is 0. This seems wrong.")

      val prices = products.map { product => 
        Price.getPricesByCode(product, Price.currentTppPriceCode).map(_.price.get) 
      }
      
      val errorMsg = s"""
        Something went wrong with price association.
          
        Error given was:
        ===============
        Penny count is 0. This seems wrong.
        ===============

        Price Info:
        ===============
        parent:
        ${refreshedUser}

        plan:
        tpp-pennyPlan
        
        pennyCount:
        ${pennyCount}
        
        rawPennyCount:
        ${rawPennyCount}
        ===============

        Products:
        ==============
        ${products}
        ==============

        Prices
        ==============
        ${prices}
        ==============
      """

      EmailActor ! SendAPIErrorEmail(errorMsg)
    }

    val (taxDue, taxRate) = {
      if (address.map(_.state.get.toLowerCase()).getOrElse("") == "ga") {
        address.map { address => 
          TaxJarService.findTaxAmoutAndRate(
            address.city.get,
            address.state.get,
            address.zip.get,
            subtotalWithDiscount
          )
        }.getOrElse((0D, 0D))
      } else {
        (0D, 0D)
      }
    }

    val stripeCustomer: Future[Box[Customer]] = Customer.create(
      email = refreshedUser.map(_.email.get),
      card = Some(stripeToken),
      coupon = couponName
    )

    stripeCustomer onComplete {
      case TrySuccess(Full(customer)) =>

        val stripeSubscription: Future[Box[StripeSubscription]] = StripeSubscription.create(
          customerId = customer.id,
          quantity = Some(pennyCount),
          coupon = Some("tpp"),
          taxPercent = Some(taxRate),
          plan = "tpp-pennyPlan"
        )

        stripeSubscription onComplete {
          case TrySuccess(Full(subscription)) =>

            val refreshedParent = refreshedUser.flatMap(_.refresh)
            val updatedParent = refreshedParent.map(_.stripeId(customer.id).saveMe)

            val subscriptionId = subscription.id.getOrElse("")

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
                if (newUser)
                  EmailActor ! NewSaleEmail(user, pets.size, "TPP")
              }

              if (newUser)
                EmailActor ! SendNewUserEmail(user)
            }

          case TrySuccess(stripeFailure) =>
            logger.error("create customer failed with: " + stripeFailure + ". Email sent to log error.")

            sendStripeErrorEmail(
              "We did not create a Stripe subscription or an internal subscription.",
              stripeFailure,
              refreshedUser,
              stripeToken,
              pennyCount,
              couponName,
              taxRate
            )

            stripeFailure
          case TryFail(throwable: Throwable) =>
            logger.error("create customer failed with: " + throwable + ". Email sent to log error.")

            sendStripeErrorEmail(
              "We did not create a Stripe subscription or an internal subscription.",
              throwable,
              refreshedUser,
              stripeToken,
              pennyCount,
              couponName,
              taxRate
            )

            throwable
        }

      case TrySuccess(stripeFailure) =>
        logger.error("create customer failed with: " + stripeFailure + ". Email sent to log error.")

        sendStripeErrorEmail(
          "We did not create a Stripe subscription or an internal subscription.",
          Left(stripeFailure),
          refreshedUser,
          stripeToken,
          pennyCount,
          couponName,
          taxRate
        )

        stripeFailure
      case TryFail(throwable: Throwable) =>
        logger.error("create customer failed with: " + throwable + ". Email sent to log error.")

        sendStripeErrorEmail(
          "We did not create a Stripe subscription or an internal subscription.",
          Right(throwable),
          refreshedUser,
          stripeToken,
          pennyCount,
          couponName,
          taxRate
        )

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
        case "xlarge" => "X-Large"
        case size => size
      }

      val product = Product.find(
        By(Product.name, sanitizedProductName),
        By(Product.sizeName, sanitizedSize)
      )

    if (product == Empty) {
      logger.error("Didnt match product. Need manual resolution. Email sent.")
      val errorMsg = s"""
        Something went wrong with product matching.
          
        We could not match the product name given. This will lead to no pets being created. And other failures.

        Error given was:
        ===============
        No product found.
        ===============

        Product Info:
        ===============
        parent:
        ${parent}

        pet:
        ${pet}
        
        rawProductName:
        ${pet.product.toLowerCase}
        
        rawPetSize:
        ${pet.currentSize.toLowerCase}
        ===============
      """

      EmailActor ! SendAPIErrorEmail(errorMsg)
    }

      val possibleWhelpDate = pet.whelpDate.getOrElse("")
      product.map(Pet.createNewPet(
        parent,
        pet.name,
        _,
        pet.breed,
        tryo(whelpDateFormat.parse(possibleWhelpDate))
      ))
    }).filter(_ != Empty)
  }

  serve {
    case req @ Req("api" :: "v1" :: "customer" :: Nil, _, PostRequest) => {
      for {
        requestBody <- (req.body ?~ "No request body." ~> 400)
        requestJson <- tryo(Serialization.read[JValue](new String(requestBody))) ?~! "Invalid JSON." ~> 400
        parentJson <- Full(requestJson \ "parent")
        possibleParent <- tryo(parentJson.extract[NewParent]) ?~ "Error in customer json." ~> 400
        petsJson <- Full(requestJson \ "pets")
        pets <- tryo(petsJson.extract[List[NewPet]]) ?~ "Error in pets json." ~> 400
        agentId <- tryo(requestJson \ "agentId").map(_.extract[String]) ?~ "Phone agent is missing." ~> 400
        } yield {
          EmailActor ! SendTppApiJsonEmail(requestJson.toString)

          val salesAgency = Agency.find(By(Agency.name, "TPP"))
          val existingUser = User.find(By(User.email, possibleParent.email), By(User.userType, UserType.Parent))

          existingUser match {
            case Empty => {
              val currentParent = User.createNewPendingUser(
                possibleParent,
                salesAgency,
                agentId
              )

              val parentAddress = Address.createNewAddress(possibleParent.address, Full(currentParent))

              val createdPets = createPets(pets, currentParent) 

              if (createdPets.size != pets.size) {
                logger.error("A pet creation failed. Emailed error.")
                
                val errorMsg = s"""
                  Something went wrong with creating a pet matching.

                  We had ${pets.size} pets in the API call but only created ${createdPets.size} new pets.

                  Error given was:
                  ===============
                  Created pets doesn't equal raw pets.
                  ===============

                  Pet Info:
                  ===============
                  parent:
                  ${currentParent}
                  
                  pets:
                  ${pets}

                  createdPets:
                  ${createdPets}
                  ===============
                """

                EmailActor ! SendAPIErrorEmail(errorMsg)
              }

              setupStripeSubscription(currentParent, possibleParent.stripeToken)

              JsonResponse(
                ("id" -> s"${currentParent.userId}") ~ ("type" -> "User"),
                Nil,
                Nil,
                201
              )
            }

            case other => {
              logger.error("Conflict on user createation. Email sent.")
              
              val errorMsg = s"""
                Something went wrong with user creation.
        
                We couldnt create a new user in our system. Nothing else will happen due to this failure.

                Error given was:
                ===============
                Matching user found for ${possibleParent.email}
                ===============

                api Info:
                ===============
                rawUser:
                ${possibleParent}
                
                existing user matched:
                ${existingUser}
                
                pets:
                ${pets}
                
                agentId:
                ${agentId}
                ===============
              """

              EmailActor ! SendAPIErrorEmail(errorMsg)

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

    case req @ Req("api" :: "v1" :: "customer" :: email :: Nil, _, DeleteRequest) => {
      for {
          sanitizedEmail <- Full(email.filterNot("\"".toSet))
          user <- (User.find(By(User.email, sanitizedEmail), By(User.userType, UserType.Parent)):Box[User]) ?~! s"User not found: $sanitizedEmail." ~> 404
        } yield {
          ParentService.removeParent(user, true)
          OkResponse()
        }
    }
  }
}
