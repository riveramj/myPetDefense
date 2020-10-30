package com.mypetdefense.snippet

import java.time.{LocalDate, ZoneId}
import java.util.Date

import net.liftweb._
import common._
import http._
import mapper._
import rest._
import util._
import Helpers._
import json._
import JsonDSL._
import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.service.{ParentService, TaxJarService}
import dispatch.Defaults._
import me.frmr.stripe.{Customer, StripeExecutor, Subscription => StripeSubscription}

import scala.concurrent.Future
import scala.util.{Failure => TryFail, Success => TrySuccess, _}

case class NewAddress(
    street1: String,
    street2: Option[String],
    city: String,
    state: String,
    zip: String
)
case class NewPet(
    name: String,
    whelpDate: Option[String],
    product: String,
    currentSize: String,
    breed: String
)
case class NewParent(
    firstName: String,
    lastName: String,
    email: String,
    address: NewAddress,
    phone: Option[String],
    stripeToken: String
)

object TPPApi extends RestHelper with Loggable {
  val stripeSecretKey: String    = Props.get("secret.key") openOr ""
  implicit val e: StripeExecutor = new StripeExecutor(stripeSecretKey)
  val tppAgency: Box[Agency]     = Agency.find(By(Agency.name, "TPP"))

  def createEventForStripeError(
      failedStepMessage: String,
      stripeFailure: Any,
      parent: Box[User],
      stripeToken: String,
      pennyCount: Int,
      couponName: Option[String],
      taxRate: Double
  ): Unit = {
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

    Event.createEvent(
      user = parent,
      eventType = EventType.Billing,
      title = "Something went wrong with stripe creation.",
      details = errorMsg
    )
  }

  def setupStripeSubscription(
      oldParent: User,
      stripeToken: String,
      newUser: Boolean = true
  ): Unit = {
    val parent                = oldParent.refresh
    val pets                  = parent.map(_.pets.toList).openOr(Nil)
    val rawPennyCount: Double = pets.size * 12.99

    val pennyCount = tryo((rawPennyCount * 100).toInt).openOr(0)

    val couponName = {
      if (pets.size > 1)
        Some(s"tpp-${pets.size}pets")
      else
        None
    }

    val address = parent.flatMap(_.addresses.toList.headOption)

    val discountAmount = (pets.size - 1) * 100

    val subtotalWithDiscount = (pennyCount - discountAmount) / 100

    if (pennyCount == 0) {
      val errorMsgTitle = "Penny count is 0. This seems wrong."
      logger.error(errorMsgTitle)

      val errorMsg = s"""
        Something went wrong with price association.
          
        Error given was:
        ===============
        Penny count is 0. This seems wrong.
        ===============

        Price Info:
        ===============
        parent:
        ${parent}

        plan:
        tpp-pennyPlan
        
        pennyCount:
        ${pennyCount}
        
        rawPennyCount:
        ${rawPennyCount}
        ===============

        Pets:
        ==============
        ${pets}
        ==============

        Prices
        ==============
        ${12.99}
        ==============
      """
      Event.createEvent(
        user = parent,
        eventType = EventType.Billing,
        title = errorMsgTitle,
        details = errorMsg
      )
    }

    val (taxDue, taxRate) = address.map { address =>
      TaxJarService.findTaxAmoutAndRate(
        address.city.get,
        address.state.get,
        address.zip.get,
        subtotalWithDiscount
      )
    }.getOrElse((0d, 0d))

    val stripeCustomer: Future[Box[Customer]] = Customer.create(
      email = parent.map(_.email.get),
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
            val refreshedParent = parent.flatMap(_.refresh)
            val updatedParent   = refreshedParent.map(_.stripeId(customer.id).saveMe)

            val subscriptionId = subscription.id.getOrElse("")

            val plusOneDayTime = LocalDate
              .now(ZoneId.of("America/New_York"))
              .plusDays(1)
              .atStartOfDay(ZoneId.of("America/New_York"))
              .toInstant()

            val plusOneDayDate = Date.from(plusOneDayTime)

            updatedParent.map { user =>
              val mpdSubscription = Subscription.createNewSubscription(
                Full(user),
                subscriptionId,
                new Date(),
                plusOneDayDate,
                Price.currentTppPriceCode
              )

              val refreshUser = user.subscription(mpdSubscription).saveMe()

              refreshUser.pets.toList.map { pet =>
                val box = SubscriptionBox.createNewBox(mpdSubscription, pet)

                pet.box(box).saveMe()
              }

              TaggedItem.createNewTaggedItem(
                subscription = Full(mpdSubscription),
                tag = Tag.useBox
              )
            }

            val coupon      = Coupon.find(By(Coupon.couponCode, couponName.getOrElse("")))
            val updatedUser = updatedParent.map(_.coupon(coupon).saveMe)

            updatedUser.map { user =>
              if (newUser)
                EmailActor ! SendNewUserEmail(user)
            }

          case TrySuccess(stripeFailure) =>
            logger.error(
              "create customer failed with: " + stripeFailure + ". Email sent to log error."
            )

            createEventForStripeError(
              "We did not create a Stripe subscription or an internal subscription.",
              stripeFailure,
              parent,
              stripeToken,
              pennyCount,
              couponName,
              taxRate
            )

            stripeFailure
          case TryFail(throwable: Throwable) =>
            logger.error("create customer failed with: " + throwable + ". Email sent to log error.")

            createEventForStripeError(
              "We did not create a Stripe subscription or an internal subscription.",
              throwable,
              parent,
              stripeToken,
              pennyCount,
              couponName,
              taxRate
            )

            throwable
        }

      case TrySuccess(stripeFailure) =>
        logger.error("create customer failed with: " + stripeFailure + ". Email sent to log error.")

        createEventForStripeError(
          "We did not create a Stripe subscription or an internal subscription.",
          Left(stripeFailure),
          parent,
          stripeToken,
          pennyCount,
          couponName,
          taxRate
        )

        stripeFailure
      case TryFail(throwable: Throwable) =>
        logger.error("create customer failed with: " + throwable + ". Email sent to log error.")

        createEventForStripeError(
          "We did not create a Stripe subscription or an internal subscription.",
          Right(throwable),
          parent,
          stripeToken,
          pennyCount,
          couponName,
          taxRate
        )

        throwable
    }
  }

  def createPets(pets: List[NewPet], parent: User): List[Box[Pet]] = {
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
        case "small"  => "Small"
        case "medium" => "Medium"
        case "large"  => "Large"
        case "xlarge" => "X-Large"
        case size     => size
      }

      val product = FleaTick.find(
        By(FleaTick.name, sanitizedProductName),
        By(FleaTick.sizeName, sanitizedSize)
      )

      if (product == Empty) {
        val errorMsgTitle = "Didn't match product. Need manual resolution."
        logger.error(errorMsgTitle)
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

        Event.createEvent(
          user = Full(parent),
          eventType = EventType.Product,
          title = errorMsgTitle,
          details = errorMsg
        )
      }

      val possibleWhelpDate = pet.whelpDate.getOrElse("")
      product.map(
        Pet.createNewPet(
          parent,
          pet.name,
          _,
          pet.breed,
          ParentService.parseWhelpDate(possibleWhelpDate)
        )
      )
    }).filter(_ != Empty)
  }

  serve {
    case req @ Req("api" :: "v1" :: "customer" :: Nil, _, PostRequest) => {
      for {
        requestBody    <- (req.body ?~ "No request body." ~> 400)
        requestJson    <- tryo(Serialization.read[JValue](new String(requestBody))) ?~! "Invalid JSON." ~> 400
        parentJson     <- Full(requestJson \ "parent")
        possibleParent <- tryo(parentJson.extract[NewParent]) ?~ "Error in customer json." ~> 400
        petsJson       <- Full(requestJson \ "pets")
        pets           <- tryo(petsJson.extract[List[NewPet]]) ?~ "Error in pets json." ~> 400
        agentId <- tryo(requestJson \ "agentId")
                    .map(_.extract[String]) ?~ "Phone agent is missing." ~> 400
        storeCode <- tryo(requestJson \ "storeCode")
                      .map(_.extract[String]) ?~ "Store code is missing." ~> 400
      } yield {
        val salesAgency = {
          val possibleAgency = Agency.find(By(Agency.storeCode, storeCode))
          if (storeCode.toLowerCase == "pupspot2")
            Agency.find(By(Agency.name, "PuppySpot"))
          else if (possibleAgency.isEmpty)
            tppAgency
          else
            possibleAgency
        }

        val backup = ApiRequestBackup.createNewBackupRecord(salesAgency, requestJson)

        val existingUser =
          User.find(By(User.email, possibleParent.email), By(User.userType, UserType.Parent))

        existingUser match {
          case Empty => {
            val currentParent = User.createNewPendingUser(
              possibleParent,
              salesAgency,
              agentId
            )

            ApiRequestBackup.updateUser(backup, currentParent)

            val parentAddress =
              Address.createNewAddress(possibleParent.address, Full(currentParent))

            val createdPets = createPets(pets, currentParent)

            if (createdPets.size != pets.size) {
              val errorMsgTitle = "A pet creation failed. Emailed error."
              logger.error(errorMsgTitle)

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

              Event.createEvent(
                user = Full(currentParent),
                eventType = EventType.Pets,
                title = errorMsgTitle,
                details = errorMsg
              )
            }

            setupStripeSubscription(currentParent, possibleParent.stripeToken)

            JsonResponse(
              ("id" -> s"${currentParent.userId}") ~ ("type" -> "User"),
              Nil,
              Nil,
              201
            )
          }

          case _ => {
            val errorMsgTitle = "Conflict on user creation."
            logger.error(errorMsgTitle)

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

            Event.createEvent(
              user = existingUser,
              eventType = EventType.User,
              title = errorMsgTitle,
              details = errorMsg
            )

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
        user <- (User.find(By(User.email, sanitizedEmail), By(User.userType, UserType.Parent)): Box[
                 User
               ]) ?~! s"User not found: $sanitizedEmail." ~> 404
      } yield {
        ParentService.removeParent(user, true)
        OkResponse()
      }
    }
  }
}
