package com.mypetdefense.snippet

import java.util.Date
import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.model.domain.action.SystemAction.SystemCanceledAccount
import com.mypetdefense.service.{StripeBoxAdapter => Stripe, _}
import com.mypetdefense.util.DateHelper.tomorrowStart
import com.mypetdefense.util.StripeHelper._
import com.stripe.param.CustomerCreateParams
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.json.JsonDSL._
import net.liftweb.json._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._

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
  def createEventForStripeError(
      failedStepMessage: String,
      stripeFailure: Any,
      parent: Box[User],
      stripeToken: String,
      items: Seq[StripeFacade.Subscription.Item],
      couponName: Option[String],
      taxRate: BigDecimal
  ): Unit = {
    val errorMsg = s"""
      Something went wrong with stripe creation.

      $failedStepMessage

      Error given was:
      ===============
      $stripeFailure
      ===============

      stripe Info:
      ===============
      parent:
      $parent

      token:
      $stripeToken

      items:
      $items

      coupon:
      $couponName

      taxPercent:
      $taxRate
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
    val parent = oldParent.reload
    val pets   = parent.pets.toList

    val catsCount = pets.count(_.animalType == AnimalType.Cat)
    val dogsCount = pets.count(_.animalType == AnimalType.Dog)

    val rawPennyCount: Double = pets.size * 12.99
    val pennyCount            = tryo((rawPennyCount * 100).toInt).openOr(0)

    val couponName = {
      if (pets.size > 1)
        Some(s"tpp-${pets.size}pets")
      else
        None
    }

    val address = parent.addresses.toList.headOption

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
        $parent

        price:
        tpp-pennyPlan
        
        pennyCount:
        $pennyCount
        
        rawPennyCount:
        $rawPennyCount
        ===============

        Pets:
        ==============
        $pets
        ==============

        Prices
        ==============
        ${12.99}
        ==============
      """
      Event.createEvent(
        user = Full(parent),
        eventType = EventType.Billing,
        title = errorMsgTitle,
        details = errorMsg
      )
    }

    val (_, taxRate) = address.map { address =>
      TaxJarService.findTaxAmountAndRate(
        address.city.get,
        address.state.get,
        address.zip.get,
        subtotalWithDiscount
      )
    }.getOrElse((0d, 0d))

    val tppPriceCode = com.mypetdefense.model.Price.currentTppPriceCode

    val subscriptionItems = ({
      import StripeFacade._

      pets.groupBy(_.size.get).map { case (size, pets) =>

        val price = com.mypetdefense.model.Price.getPricesByCodeBySize(
          tppPriceCode,
          size,
          BoxType.basic
        )

        val priceId = price.map(_.stripePriceId.get).getOrElse("")

        Subscription.Item(priceId, pets.size)
      }
    }).toList

    val stripeCustomer =
      Stripe.Customer.create(
        CustomerCreateParams.builder
          .setEmail(oldParent.email.get)
          .setSource(stripeToken)
          .whenDefined(couponName)(_.setCoupon)
          .build
      )

    stripeCustomer match {
      case Full(customer) =>
        val stripeSubscription =
          StripeFacade.Subscription.createWithTaxRate(
            customer,
            taxRate,
            coupon = Some("tpp"),
            subscriptionItems
          )

        stripeSubscription match {
          case Full(subscription) =>
            val refreshedParent = parent.reload
            val updatedParent   = refreshedParent.stripeId(customer.id).saveMe

            val mpdSubscription = Subscription.createNewSubscription(
              Full(updatedParent),
              subscription.id,
              new Date(),
              tomorrowStart,
              Price.currentTppPriceCode
            )

            val refreshUser = updatedParent.subscription(mpdSubscription).saveMe()

            refreshUser.pets.toList.map { pet =>
              val box = SubscriptionBox.createNewBox(mpdSubscription, pet, BoxType.basic)

              pet.box(box).saveMe()
            }

            TaggedItem.createNewTaggedItem(
              subscription = Full(mpdSubscription),
              tag = Tag.useBox
            )

            val coupon      = Coupon.find(By(Coupon.couponCode, couponName.getOrElse("")))
            val updatedUser = updatedParent.coupon(coupon).saveMe

            if (newUser) EmailActor ! SendNewUserEmail(updatedUser)

          case stripeFailure =>
            logger.error(
              s"create customer failed with: $stripeFailure. Email sent to log error."
            )

            createEventForStripeError(
              "We did not create a Stripe subscription or an internal subscription.",
              stripeFailure,
              Full(parent),
              stripeToken,
              subscriptionItems,
              couponName,
              taxRate
            )
        }

      case stripeFailure =>
        logger.error(s"create customer failed with: $stripeFailure. Email sent to log error.")

        createEventForStripeError(
          "We did not create a Stripe subscription or an internal subscription.",
          stripeFailure,
          Full(parent),
          stripeToken,
          subscriptionItems,
          couponName,
          taxRate
        )
    }
  }

  def createPets(pets: List[NewPet], parent: User): List[Box[Pet]] = {
    pets.map { pet =>
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
        $parent

        pet:
        $pet

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
    }.filter(_ != Empty)
  }

  serve {
    case req @ Req("api" :: "v1" :: "customer" :: Nil, _, PostRequest) =>
      for {
        requestBody    <- req.body ?~ "No request body." ~> 400
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
            Agency.tppAgency
          else
            possibleAgency
        }

        val backup = ApiRequestBackup.createNewBackupRecord(salesAgency, requestJson)

        val existingUser =
          User.find(By(User.email, possibleParent.email), By(User.userType, UserType.Parent))

        existingUser match {
          case Empty =>
            val currentParent = User.createNewPendingUser(
              possibleParent,
              salesAgency,
              agentId
            )

            ApiRequestBackup.updateUser(backup, currentParent)

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
                  $currentParent

                  pets:
                  $pets

                  createdPets:
                  $createdPets
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

          case _ =>
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
                $possibleParent

                existing user matched:
                $existingUser

                pets:
                $pets

                agentId:
                $agentId
                ===============
              """

            Event.createEvent(
              user = existingUser,
              eventType = EventType.User,
              title = errorMsgTitle,
              details = errorMsg
            )

            JsonResponse(
              "message" -> "possible duplicate parent found. data logged",
              Nil,
              Nil,
              200
            )
        }
      }

    case Req("api" :: "v1" :: "customer" :: email :: Nil, _, DeleteRequest) =>
      for {
        sanitizedEmail <- Full(email.filterNot("\"".toSet))
        user <- (User.find(By(User.email, sanitizedEmail), By(User.userType, UserType.Parent)): Box[
                 User
               ]) ?~! s"User not found: $sanitizedEmail." ~> 404
      } yield {
        val actionLog = SystemCanceledAccount(
          user.userId.get,
          None,
          user.subscription.obj.map(_.subscriptionId.get).openOr(0L)
        )
        ParentService.removeParent(user, actionLog, fullDelete = true)
        OkResponse()
      }
  }
}
