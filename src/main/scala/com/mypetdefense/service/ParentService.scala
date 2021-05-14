package com.mypetdefense.service

import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.model.domain.action.Action
import com.mypetdefense.model.domain.action.CustomerAction.CustomerAddedPet
import com.mypetdefense.model.domain.action.SupportAction.SupportAddedPet
import com.mypetdefense.service.{StripeBoxAdapter => Stripe}
import com.mypetdefense.shipstation.Order
import com.mypetdefense.snippet.TPPApi
import com.mypetdefense.util.StripeHelper._
import com.stripe.param._
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import net.liftweb.util._

import java.time.{LocalDate, Period, ZoneId}
import java.util.Date

object ParentService extends LoggableBoxLogging {
  val whelpDateFormat                  = new java.text.SimpleDateFormat("M/d/y")
  val currentPetlandPrice: String      = Props.get("petland.6month.payment") openOr ""
  val petlandMonthlyPrice: Box[String] = Props.get("petland.1month.payment")
  val petland5MonthCoupon: Box[String] = Props.get("petland.5month.coupon")

  def updateStripeCustomerCard(customerId: String, stripeToken: String, user: User): Unit = {
    if (customerId == "") {
      TPPApi.setupStripeSubscription(
        user,
        stripeToken,
        newUser = false
      )
    } else {
      val params = CustomerUpdateParams.builder.setSource(stripeToken).build
      StripeFacade.Customer
        .update(customerId, params)
        .logFailure("update customer failed. Email sent to log error.")
    }
  }

  def updateStripeSubscriptionQuantity(
      subscriptionId: String,
      productId: String,
      quantity: Int
  ): Box[Stripe.Subscription] = {

    val params =
      SubscriptionItemUpdateParams.builder
        .setQuantity(quantity)
        .setProrationBehavior(SubscriptionItemUpdateParams.ProrationBehavior.NONE)
        .build

    StripeFacade.Subscription
      .updateSubscriptionItem(subscriptionId, params, productId)
      .logFailure("update subscription failed with stripe error")
  }

  def updateCoupon(customerId: String, couponCode: Box[String]): Box[Stripe.Customer] = {
    couponCode.toOption.fold {
      StripeFacade.Customer.deleteDiscount(customerId).flatMap[Stripe.Customer](_ => Empty)
    } { coupon =>
      val params = CustomerUpdateParams.builder.setCoupon(coupon).build
      StripeFacade.Customer.update(customerId, params)
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

  def removeParent(oldUser: User, actionLog: Action, fullDelete: Boolean = false): Box[Any] = {
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
      StripeFacade.Customer
        .delete(stripeCustomerId)
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
          ActionLogService.logAction(actionLog)

          Full {
            user.pets.toList.map(_.delete_!)
            user.delete_!
          }
        } else {
          user.cancel
          paidOpenShipments.map(shipment => refundShipment(shipment, Full(user)))
          addresses.map(_.cancel)
          ActionLogService.logAction(actionLog)

          subscription.map(_.cancel)
        }


      case _ =>
        user.cancel
        paidOpenShipments.map(shipment => refundShipment(shipment, Full(user)))
        addresses.map(_.cancel)
        subscription.map(_.cancel)

        ActionLogService.logAction(actionLog)

        Empty
    }
  }

  def getStripeCustomer(customerId: String): Box[Stripe.Customer] =
    Stripe.Customer
      .retrieve(customerId)
      .logFailure("get customer failed with stripe error")

  def getStripeCustomerWithSources(customerId: String): Box[StripeFacade.CustomerWithSources] =
    StripeFacade.Customer
      .retrieveWithSources(customerId)
      .logFailure("get customer failed with stripe error")

  def getStripeCustomerDiscount(customerId: String): Box[Stripe.Discount] =
    getStripeCustomer(customerId).flatMap(_.discount)

  def getDiscount(customerId: String): Box[Int] =
    for {
      discount   <- getStripeCustomerDiscount(customerId)
      coupon     <- discount.coupon
      percentOff <- coupon.percentOff
    } yield percentOff.toInt

  def getUpcomingInvoice(customerId: String): Box[Stripe.Invoice] = {
    val params = InvoiceUpcomingParams.builder.setCustomer(customerId).build
    Stripe.Invoice
      .upcoming(params)
      .logFailure("get upcoming invoice failed with stripe error")
  }

  def getInvoice(invoiceId: String): Box[Stripe.Invoice] =
    Stripe.Invoice
      .retrieve(invoiceId)
      .logFailure("get invoice failed with stripe error")

  def getStripeSubscription(subscriptionId: String): Box[Stripe.Subscription] =
    Stripe.Subscription
      .retrieve(subscriptionId)
      .logFailure("get subscription failed with stripe error")

  def getCustomerCard(customerId: String): Box[Stripe.Card] =
    for {
      customer <- getStripeCustomerWithSources(customerId)
      sources  <- customer.value.sources
      source   <- sources.data.headOption
      if source.isCard
    } yield source.asCard

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

  def updateNextShipDate(subscription: Subscription): Box[Subscription] = {
    val stripeSubscriptionId = subscription.stripeSubscriptionId.get

    getStripeSubscription(stripeSubscriptionId) match {
      case Full(stripeSubscription) =>
        val currentPeriodEnd = stripeSubscription.currentPeriodEnd.getOrElse(0L)
        val nextMonthDate    = new Date(currentPeriodEnd * 1000L)

        Full(subscription.nextShipDate(nextMonthDate).saveMe())

      case _ => Empty
    }
  }

  def changeStripeBillDate(subscriptionId: String, date: Long): Box[Stripe.Subscription] = {

    val params =
      SubscriptionUpdateParams.builder
        .setTrialEnd(date)
        .setProrationBehavior(SubscriptionUpdateParams.ProrationBehavior.NONE)
        .build

    //TODO actually use this result or do something, not just yelling into the void
    StripeFacade.Subscription
      .update(subscriptionId, params)
      .logFailure(
        s"update subscription [subscriptionId=$subscriptionId, date=$date] failed with stripe error"
      )
  }

  def notTrialSubscription_?(subscriptionId: String): Boolean = {
    val subscription = getStripeSubscription(subscriptionId)
    val trialStatus  = subscription.flatMap(_.status).getOrElse("")

    trialStatus != "trialing"
  }

  def chargeStripeCustomer(
      amount: Long,
      stripeCustomerId: Option[String],
      description: String
  ): Box[Stripe.Charge] =
    createStripeCharge {
      ChargeCreateParams.builder
        .setAmount(amount)
        .setCurrency("USD")
        .setDescription(description)
        .setCapture(true)
        .whenDefined(stripeCustomerId)(_.setCustomer)
        .build
    }

  def chargeStripeCustomerNewCard(
      amount: Long,
      stripeCustomerId: Option[String],
      stripeToken: String,
      internalDescription: String
  ): Box[Stripe.Charge] = {
    val newCard = StripeFacade.Customer
      .createCard(stripeCustomerId.getOrElse(""), stripeToken)
      .logFailure("create card failed with stripe error")

    val cardId = newCard.map(_.id)

    createStripeCharge {
      ChargeCreateParams.builder
        .setAmount(amount)
        .setCurrency("USD")
        .setDescription(internalDescription)
        .setCapture(true)
        .whenDefined(stripeCustomerId)(_.setCustomer)
        .whenDefined(cardId)(_.setSource)
        .build
    }
  }

  def chargeGuestCard(
      amount: Long,
      stripeToken: String,
      internalDescription: String
  ): Box[Stripe.Charge] =
    createStripeCharge {
      ChargeCreateParams.builder
        .setAmount(amount)
        .setCurrency("USD")
        .setSource(stripeToken)
        .setDescription(internalDescription)
        .setCapture(true)
        .build
    }

  private def createStripeCharge(params: ChargeCreateParams): Box[Stripe.Charge] =
    Stripe.Charge
      .create(params)
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
                 isUpgraded: Boolean,
                 actionLog: Either[CustomerAddedPet,SupportAddedPet],
                 breed: String = "",
                 birthday: String = "",
                 chosenMonthlySupplement: Box[Product] = Empty
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
    val updatedActionLog = actionLog match {
      case Left(customerAddedPet) =>
        customerAddedPet.copy(petId = newPet.petId.get)
      case Right(supportAddedPet) =>
        supportAddedPet.copy(petId = newPet.petId.get)
    }

    val updatedPet = oldUser.subscription.obj.map { subscription =>
      val box = SubscriptionBox.createNewBox(subscription, newPet, isUpgraded, chosenMonthlySupplement.isDefined)

      if (newPet.animalType.get == AnimalType.Dog && chosenMonthlySupplement.isEmpty)
        SubscriptionItem.createFirstBox(box, false)
      else if (newPet.animalType.get == AnimalType.Dog && chosenMonthlySupplement.isDefined)
        SubscriptionItem.createNewBox(PendingPet(newPet, chosenMonthlySupplement, Full(box)))

      newPet.box(box).saveMe()
    }

    val updatedSubscription = updateStripeSubscriptionTotal(oldUser, updatedPet)

    updatedSubscription match {
      case Full(_) =>
        ActionLogService.logAction(updatedActionLog)
        Full(newPet)
      case _       => Empty
    }
  }

  def updateStripeSubscriptionTotal(
    oldUser: User,
    petToUpdate: Box[Pet] = Empty
  ): Box[Stripe.Subscription] = {
    val updatedUser       = oldUser.reload
    val maybeSubscription = updatedUser.subscription.obj

    val priceDetails = (for {
      subscription <- maybeSubscription
      pet <- petToUpdate
      box <- pet.box.obj
      product <- box.fleaTick.obj
      petSize = product.size.get
      priceCode = subscription.priceCode.get
      isUpgraded = subscription.isUpgraded.get
    } yield {
      val boxType = if (isUpgraded) BoxType.healthAndWellness else BoxType.basic

      Price.getPricesByCodeBySize(priceCode, petSize, boxType)
    }).flatten

    val sizedBoxes = for {
      subscription <- maybeSubscription.toList
      pet <- petToUpdate.toList
      box <- subscription.subscriptionBoxes.toList
        if box.status.get == Status.Active
      fleaTick <- box.fleaTick.obj.toList
        if fleaTick.size.get == pet.size.get
    } yield box

    updateStripeSubscriptionQuantity(
      maybeSubscription.map(_.stripeSubscriptionId.get).openOr(""),
      priceDetails.map(_.stripeProductId.get).openOr(""),
      sizedBoxes.size
    ).logEmptyBox("update stripe subscription total failed")
  }

  def removePet(oldUser: Box[User], pet: Pet, actionLog: Action): Box[Pet] = {
    oldUser.flatMap(user => removePet(user, pet, actionLog))
  }

  def removePet(oldUser: Box[User], oldPet: Box[Pet], actionLog: Action): Box[Pet] =
    (for {
      user <- oldUser
      pet  <- oldPet
    } yield removePet(user, pet, actionLog)).flatten

  def removePet(oldUser: User, oldPet: Pet, actionLog: Action): Box[Pet] = {
    val refreshedPet = Pet.find(By(Pet.petId, oldPet.petId.get))
    val updatedPet   = refreshedPet.map(_.status(Status.Cancelled).saveMe)

    for {
      pet <- updatedPet
      box <- pet.box
    } {
      box.status(Status.Cancelled).saveMe
    }

    val updatedSubscription = updateStripeSubscriptionTotal(oldUser, Full(oldPet))

    val updatedUser = oldUser.reload

    updatedSubscription match {
      case Full(_) =>
        if (updatedUser.activePets.isEmpty) {
          val subscription = updatedUser.subscription.obj
          subscription.map(_.status(Status.UserSuspended).saveMe)
        }

        ActionLogService.logAction(actionLog)

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

  def updateTaxRate(subscriptionId: String, taxRate: Double): Box[Stripe.Subscription] = {
    //TODO actually use this result or do something, not just yelling into the void
    StripeFacade.Subscription
      .replaceTaxRate(subscriptionId, taxRate)
      .logFailure("update subscription tax rate failed with stripe error")
  }

  def getStripeProductPrice(priceId: String): Box[Stripe.Price] =
    Stripe.Price
      .retrieve(priceId)
      .logFailure("get price failed with stripe error")

  def getCurrentPetlandProductPrice: Box[Stripe.Price] =
    getStripeProductPrice(currentPetlandPrice)

  /*
  def changeToPetlandMonthlyStripePrice(subscriptionId: String): Box[Stripe.Subscription] = {
    def subscriptionUpdate: Box[Stripe.Subscription] = {

      val params =
        SubscriptionUpdateParams.builder
          .setProrationBehavior(ProrationBehavior.NONE)
          .whenDefined(petland5MonthCoupon)(_.setCoupon)
          .build

      StripeFacade.Subscription.update(subscriptionId, params)
    }


    def subscriptionItemUpdate(subscription: Stripe.Subscription): Box[Stripe.Subscription] = {

      val params =
        SubscriptionItemUpdateParams.builder
          .setProrationBehavior(ProrationBehavior.NONE)
          .whenDefined(petlandMonthlyPrice)(_.setPrice)
          .build

      StripeFacade.Subscription.updateFirstItem(subscription, params)
    }

    subscriptionUpdate
      .flatMap(subscriptionItemUpdate)
      .logFailure("update subscription failed with stripe error")
  }
  */

  def refundShipment(shipment: Shipment, possibleParent: Box[User] = Empty): Box[Stripe.Refund] = {
    val parent =
      if (possibleParent.isEmpty) shipment.subscription.obj.flatMap(_.user.obj)
      else possibleParent

    val params = RefundCreateParams.builder.setCharge(shipment.stripeChargeId.get).build
    val refund = Stripe.Refund
      .create(params)
      .logFailure("create refund failed with stripe error")

    refund foreach { _ =>
      shipment.dateRefunded(new Date()).saveMe
      EmailActor ! SendShipmentRefundedEmail(parent, shipment)
      ShipStationService.cancelShipstationOrder(shipment)
    }

    refund
  }
}

sealed trait PetAction extends Product with Serializable
case object AddPet extends PetAction
case object RemovePet extends PetAction
