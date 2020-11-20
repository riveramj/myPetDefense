package com.mypetdefense.service

import java.time.{LocalDate, Period, ZoneId}
import java.util.Date

import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.shipstation.Order
import com.mypetdefense.snippet.TPPApi
import com.mypetdefense.util.StripeHelper._
import com.stripe.model.{Subscription => StripeSubscription, _}
import net.liftweb.common.BoxLogging._
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import net.liftweb.util._

import scala.collection.JavaConverters._

object ParentService extends Loggable {
  val whelpDateFormat                  = new java.text.SimpleDateFormat("M/d/y")
  val currentPentlandPlan: String      = Props.get("petland.6month.payment") openOr ""
  val petlandMonthlyPlan: Box[String]  = Props.get("petland.1month.payment")
  val petland5MonthCoupon: Box[String] = Props.get("petland.5month.coupon")

  def updateStripeCustomerCard(customerId: String, stripeToken: String, user: User): Unit = {
    if (customerId == "") {
      TPPApi.setupStripeSubscription(
        user,
        stripeToken,
        newUser = false
      )
    } else {
      updateCustomer(customerId, ParamsMap("card" --> stripeToken))
        .logFailure("update customer failed. Email sent to log error.")
    }
  }

  def updateStripeSubscriptionQuantity(
      subscriptionId: String,
      quantity: Int
  ): Box[StripeSubscription] = {
    val params = ParamsMap(
      "quantity" --> quantity,
      "prorate" --> false
    )

    updateSubscription(subscriptionId, params)
      .logFailure("update subscription failed with stripe error")
  }

  def updateCoupon(customerId: String, couponCode: Box[String]): Box[Customer] = {
    couponCode.toOption.fold { deleteCustomerDiscount(customerId).flatMap[Customer](_ => Empty) } {
      coupon => updateCustomer(customerId, ParamsMap("coupon" --> coupon))
    }
  }

  def cancelOpenOrders(oldUser: User): List[Box[Order]] = {
    val openShipments =
      for {
        subscription <- oldUser.subscription.toList
        shipment     <- subscription.shipments
        if shipment.dateShipped.get == null && shipment.shipStationOrderId.get > 0
      } yield shipment

    openShipments.map(ShipStationService.cancelShipstationOrder)
  }

  def removeParent(oldUser: User, fullDelete: Boolean = false): Box[Any] = {
    val user             = oldUser.reload
    val stripeCustomerId = user.stripeId.get

    val subscription = user.subscription.obj
    val paidOpenShipments = subscription
      .map(_.shipments.toList)
      .openOr(Nil)
      .filter(_.shipmentStatus.get == ShipmentStatus.Paid)
    val addresses = user.addresses.toList

    cancelOpenOrders(oldUser)

    val deletedCustomer =
      deleteCustomer(stripeCustomerId)
        .logFailure("remove customer failed with stripe error")

    deletedCustomer match {
      case Full(_) =>
        if (fullDelete) {
          subscription.map(_.shipments.toList).openOr(Nil).map { shipment =>
            shipment.shipmentLineItems.map(_.delete_!)
            shipment.delete_!
          }
          addresses.map(_.delete_!)
          subscription.map(_.delete_!)
          Full {
            user.pets.toList.map(_.delete_!)
            user.delete_!
          }
        } else {
          user.cancel
          paidOpenShipments.map(shipment => refundShipment(shipment, Full(user)))
          addresses.map(_.cancel)
          subscription.map(_.cancel)
        }

      case _ =>
        user.cancel
        paidOpenShipments.map(shipment => refundShipment(shipment, Full(user)))
        addresses.map(_.cancel)
        subscription.map(_.cancel)

        Empty
    }
  }

  def getStripeCustomer(customerId: String): Box[Customer] =
    Box.tryo { Customer.retrieve(customerId) }
      .logFailure("get customer failed with stripe error")

  def getStripeCustomerDiscount(customerId: String): Box[Discount] =
    getStripeCustomer(customerId).flatMap(c => Option(c.getDiscount))

  def getDiscount(customerId: String): Box[Int] =
    for {
      discount   <- getStripeCustomerDiscount(customerId)
      coupon     <- Option(discount.getCoupon)
      percentOff <- Option(coupon.getPercentOff)
    } yield percentOff.toInt

  def getUpcomingInvoice(customerId: String): Box[Invoice] =
    Box.tryo { Invoice.upcoming(ParamsMap("customer" --> customerId)) }
      .logFailure("get upcoming invoice failed with stripe error")

  def getInvoice(invoiceId: String): Box[Invoice] =
    Box.tryo { Invoice.retrieve(invoiceId) }
      .logFailure("get invoice failed with stripe error")

  def getStripeSubscription(subscriptionId: String): Box[StripeSubscription] =
    Box.tryo { StripeSubscription.retrieve(subscriptionId) }
      .logFailure("get subscription failed with stripe error")

  def getCustomerCard(customerId: String): Option[Card] =
    for {
      customer    <- getStripeCustomer(customerId).toOption
      sources     <- Option(customer.getSources)
      sourcesData <- Option(sources.getData)
      source      <- sourcesData.asScala.headOption
      if source.getObject == "card"
    } yield source.asInstanceOf[Card]

  def updateNextShipBillDate(subscription: Subscription, nextDate: Date): Box[Subscription] = {
    val updatedSubscription = changeStripeBillDate(
      subscription.stripeSubscriptionId.get,
      nextDate.getTime / 1000
    )

    updatedSubscription match {
      case Full(_) => Full(subscription.nextShipDate(nextDate).saveMe)
      case _       => Empty
    }
  }

  def updateNextShipDate(subscription: Subscription): Serializable = {
    val stripeSubscriptionId = subscription.stripeSubscriptionId.get

    getStripeSubscription(stripeSubscriptionId) match {
      case Full(stripeSubscription) =>
        val currentPeriodEnd = Option(stripeSubscription.getCurrentPeriodEnd).fold(0L)(_.toLong)
        val nextMonthDate    = new Date(currentPeriodEnd * 1000L)

        val nextMonthLocalDate =
          nextMonthDate.toInstant.atZone(ZoneId.of("America/New_York")).toLocalDate

        val startOfDayDate = nextMonthLocalDate
          .atStartOfDay(ZoneId.of("America/New_York"))
          .toInstant
          .getEpochSecond

        changeStripeBillDate(subscription.stripeSubscriptionId.get, startOfDayDate)

        subscription.nextShipDate(nextMonthDate).saveMe

      case _ => Empty
    }
  }

  def changeStripeBillDate(subscriptionId: String, date: Long): Box[StripeSubscription] = {
    //TODO actually use this result or do something, not just yelling into the void
    updateSubscription(subscriptionId, ParamsMap("trial_end" --> date, "prorate" --> false))
      .logFailure("update subscription failed with stripe error")
  }

  def notTrialSubscription_?(subscriptionId: String): Boolean = {
    val subscription = getStripeSubscription(subscriptionId)
    val trialStatus  = subscription.flatMap(s => Option(s.getStatus)).getOrElse("")

    trialStatus != "trialing"
  }

  def chargeStripeCustomer(
      amount: Long,
      stripeCustomerId: Option[String],
      description: String
  ): Box[Charge] =
    createStripeCharge {
      ParamsMap(
        "amount" --> amount,
        "currency" --> "USD",
        "customer" -?> stripeCustomerId,
        "description" --> description,
        "capture" --> true
      )
    }

  def chargeStripeCustomerNewCard(
      amount: Long,
      stripeCustomerId: Option[String],
      stripeToken: String,
      internalDescription: String
  ): Box[Charge] = {
    val newCard = createCustomerCard(stripeCustomerId.getOrElse(""), stripeToken)
      .logFailure("create card failed with stripe error")

    val cardId = newCard.flatMap(c => Option(c.getId))

    createStripeCharge {
      ParamsMap(
        "amount" --> amount,
        "currency" --> "USD",
        "customer" -?> stripeCustomerId,
        "card" -?> cardId,
        "description" --> internalDescription,
        "capture" --> true
      )
    }
  }

  def chargeGuestCard(
      amount: Long,
      stripeToken: String,
      internalDescription: String
  ): Box[Charge] =
    createStripeCharge {
      ParamsMap(
        "amount" --> amount,
        "currency" --> "USD",
        "card" --> stripeToken,
        "description" --> internalDescription,
        "capture" --> true
      )
    }

  private def createStripeCharge(params: ParamsMap): Box[Charge] =
    Box.tryo { Charge.create(params) }
      .logFailure("charge customer failed with stripe error")

  def parseWhelpDate(whelpDate: String): Box[Date] = {
    val whelpDateFormats = List(
      new java.text.SimpleDateFormat("M/d/y"),
      new java.text.SimpleDateFormat("y-M-d")
    )

    whelpDateFormats.map { dateFormat => tryo(dateFormat.parse(whelpDate)) }
      .find(_.isDefined)
      .getOrElse(Empty)
  }

  def addNewPet(
      oldUser: User,
      name: String,
      animalType: AnimalType.Value,
      size: AnimalSize.Value,
      product: FleaTick,
      breed: String = "",
      birthday: String = ""
  ): Box[Pet] = {

    val possibleBirthday = parseWhelpDate(birthday)

    val newPet = Pet.createNewPet(
      user = oldUser,
      name = name,
      animalType = animalType,
      size = size,
      whelpDate = possibleBirthday,
      breed = breed
    )

    oldUser.subscription.obj.map { subscription =>
      val box = SubscriptionBox.createBasicBox(subscription, product, newPet)

      newPet.box(box).saveMe()
    }

    val updatedSubscription = updateStripeSubscriptionTotal(oldUser)

    updatedSubscription match {
      case Full(_) => Full(newPet)
      case _       => Empty
    }
  }

  def updateStripeSubscriptionTotal(oldUser: User): Box[StripeSubscription] = {
    val updatedUser       = oldUser.reload
    val maybeSubscription = updatedUser.subscription.obj

    val products: List[FleaTick] = {
      for {
        subscription <- maybeSubscription.toList
        boxes        <- subscription.subscriptionBoxes
        fleaTick     <- boxes.fleaTick
      } yield {
        fleaTick
      }
    }

    val (subscriptionId, priceCode) = (
      for {
        subscription <- updatedUser.subscription.obj
      } yield {
        (subscription.stripeSubscriptionId.get, subscription.priceCode.get)
      }
    ).openOr(("", ""))

    val prices: List[Double] = products.map { product =>
      Price.getPricesByCode(product, priceCode).map(_.price.get).openOr(0d)
    }

    val totalCost = "%.2f".format(prices.foldLeft(0d)(_ + _)).toDouble

    updateStripeSubscriptionQuantity(
      subscriptionId,
      tryo((totalCost * 100).toInt).openOr(0)
    )
  }

  def removePet(oldUser: Box[User], pet: Pet): Box[Pet] = {
    oldUser.flatMap(user => removePet(user, pet))
  }

  def removePet(oldUser: Box[User], oldPet: Box[Pet]): Box[Pet] =
    (for {
      user <- oldUser
      pet  <- oldPet
    } yield removePet(user, pet)).flatten

  def removePet(oldUser: User, oldPet: Pet): Box[Pet] = {
    val refreshedPet = Pet.find(By(Pet.petId, oldPet.petId.get))
    val updatedPet   = refreshedPet.map(_.status(Status.Cancelled).saveMe)

    val updatedSubscription = updateStripeSubscriptionTotal(oldUser)

    val updatedUser = oldUser.reload

    updatedSubscription match {
      case Full(_) =>
        if (updatedUser.activePets.isEmpty) {
          val subscription = updatedUser.subscription.obj
          subscription.map(_.status(Status.UserSuspended).saveMe)
        }

        updatedPet

      case _ =>
        Empty
    }
  }

  def getGrowthMonthNumber(growthRate: Box[GrowthRate], size: String): Int = {
    size match {
      case "medium" =>
        growthRate.map(_.mediumProductMonth.get).openOr(0)
      case "large" =>
        growthRate.map(_.largeProductMonth.get).openOr(0)
      case "xlarge" =>
        growthRate.map(_.xlargeProductMonth.get).openOr(0)
      case _ =>
        0
    }
  }

  def findGrowthMonth(pet: Pet): Int = {
    val currentDate = LocalDate.now()

    val birthday = tryo(pet.birthday.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate)

    val currentMonth = birthday.map(Period.between(_, currentDate).getMonths).openOr(0)

    val growthDelay = tryo(pet.nextGrowthDelay.get).openOr(0)

    currentMonth - growthDelay
  }

  def checkForNewProduct(
      pet: Pet,
      box: SubscriptionBox,
      newProduct: Box[FleaTick],
      user: User
  ): Box[(Pet, String, User)] = {
    if (box.fleaTick.obj != newProduct)
      Full((pet, newProduct.map(_.getNameAndSize).openOr(""), user))
    else
      Empty
  }

  def findGrowingPets(subscription: Subscription): Seq[(Pet, String, User)] = {
    (for {
      user     <- subscription.user.obj.toList
      box      <- subscription.subscriptionBoxes
      fleaTick <- box.fleaTick.obj
      if fleaTick.isZoGuard_?
      pet <- box.pet.obj
      if (pet.breed.get != null) && (pet.birthday.get != null)
    } yield {
      val growthRate = GrowthRate.find(By(GrowthRate.breed, pet.breed.get.toLowerCase))

      val growthMonth = findGrowthMonth(pet)

      growthMonth match {
        case medium if medium == getGrowthMonthNumber(growthRate, "medium") =>
          val newProduct = FleaTick.find(By(FleaTick.size, AnimalSize.DogMediumZo))
          checkForNewProduct(pet, box, newProduct, user)

        case large if large == getGrowthMonthNumber(growthRate, "large") =>
          val newProduct = FleaTick.find(By(FleaTick.size, AnimalSize.DogLargeZo))
          checkForNewProduct(pet, box, newProduct, user)

        case xlarge if xlarge == getGrowthMonthNumber(growthRate, "xlarge") =>
          val newProduct = FleaTick.find(By(FleaTick.size, AnimalSize.DogXLargeZo))
          checkForNewProduct(pet, box, newProduct, user)

        case _ =>
          Empty
      }
    }).flatten
  }

  def updatePuppyProducts(user: User): List[Any] = {
    for {
      subscription <- user.subscription.toList
      box          <- subscription.subscriptionBoxes
      fleaTick     <- box.fleaTick
      if fleaTick.isZoGuard_?
      pet <- box.pet
      if (pet.breed.get != null) &&
        (pet.birthday.get != null)
    } yield {
      val growthRate = GrowthRate.find(By(GrowthRate.breed, pet.breed.get.toLowerCase))

      val growthMonth = findGrowthMonth(pet)

      growthMonth match {
        case medium if medium == getGrowthMonthNumber(growthRate, "medium") =>
          val newProduct = FleaTick.find(By(FleaTick.size, AnimalSize.DogMediumZo))
          newProduct.map { product =>
            box
              .fleaTick(product)
              .saveMe

            pet
              .size(AnimalSize.DogMediumZo)
              .nextGrowthDelay(0)
              .saveMe
          }

        case large if large == getGrowthMonthNumber(growthRate, "large") =>
          val newProduct = FleaTick.find(By(FleaTick.size, AnimalSize.DogLargeZo))
          newProduct.map { product =>
            box
              .fleaTick(product)
              .saveMe

            pet
              .size(AnimalSize.DogLargeZo)
              .nextGrowthDelay(0)
              .saveMe
          }

        case xlarge if xlarge == getGrowthMonthNumber(growthRate, "xlarge") =>
          val newProduct = FleaTick.find(By(FleaTick.size, AnimalSize.DogXLargeZo))
          newProduct.map { product =>
            box
              .fleaTick(product)
              .saveMe

            pet
              .size(AnimalSize.DogXLargeZo)
              .nextGrowthDelay(0)
              .saveMe
          }

        case _ =>
      }
    }
  }

  def updateTaxRate(subscriptionId: String, taxRate: Double): Box[StripeSubscription] = {
    //TODO actually use this result or do something, not just yelling into the void
    updateSubscription(subscriptionId, ParamsMap("tax_percent" --> taxRate))
      .logFailure("update subscription tax rate failed with stripe error")
  }

  def getStripeProductPlan(planId: String): Box[Plan] =
    Box.tryo { Plan.retrieve(planId) }
      .logFailure("get plan failed with stripe error")

  def getCurrentPetlandProductPlan: Box[Plan] =
    getStripeProductPlan(currentPentlandPlan)

  def changeToPetlandMonthlyStripePlan(subscriptionId: String): Box[StripeSubscription] = {
    val updateParams = ParamsMap(
      "prorate" --> false,
      "plan" -?> petlandMonthlyPlan,
      "coupon" -?> petland5MonthCoupon
    )

    updateSubscription(subscriptionId, updateParams)
      .logFailure("update subscription failed with stripe error")
  }

  def refundShipment(shipment: Shipment, possibleParent: Box[User] = Empty): Box[Refund] = {
    val parent =
      if (possibleParent.isEmpty) shipment.subscription.obj.flatMap(_.user.obj)
      else possibleParent

    val refund = Box.tryo { Refund.create(ParamsMap("charge" --> shipment.stripeChargeId.get)) }
      .logFailure("create refund failed with stripe error")

    refund foreach { _ =>
      shipment.dateRefunded(new Date()).saveMe
      EmailActor ! SendShipmentRefundedEmail(parent, shipment)
      ShipStationService.cancelShipstationOrder(shipment)
    }

    refund
  }
}
