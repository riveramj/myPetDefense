package com.mypetdefense.service

import net.liftweb._
  import common._
  import mapper._
  import util._
  import util.Helpers._

import me.frmr.stripe.{Coupon => StripeCoupon, Subscription => StripeSubscription, Product => StripeProduct, _}

import scala.util.{Failure => TryFail, Success => TrySuccess, _}
import com.mypetdefense.snippet.TPPApi

import com.mypetdefense.model._

import scala.concurrent.Await
import scala.concurrent.duration._

import dispatch._, Defaults._

import java.util.Date
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import java.time.{LocalDate, ZoneId, LocalDateTime, Period}
import java.time.format.DateTimeFormatter

object ParentService extends Loggable {
  val stripeSecretKey = Props.get("secret.key") openOr ""
  implicit val e = new StripeExecutor(stripeSecretKey)

  val whelpDateFormat = new java.text.SimpleDateFormat("M/d/y")

  def updateStripeCustomerCard(customerId: String, stripeToken: String, user: User) = {
    if (customerId == "") {
      TPPApi.setupStripeSubscription(
        user,
        stripeToken,
        false
      )
    } else {
      Customer.update(
        id = customerId,
        card = Some(stripeToken)
      ) onComplete {
        case TrySuccess(Full(customer)) =>
        case TrySuccess(stripeFailure) =>
          logger.error("update customer failed with: " + stripeFailure + ". Email sent to log error.")
          
        case TryFail(throwable: Throwable) =>
          logger.error("update customer failed with: " + throwable + ". Email sent to log error.")
      }
    }
  }

  def updateStripeSubscriptionQuantity(customerId: String, subscriptionId: String, quantity: Int): Box[StripeSubscription] = {
    val subscription = StripeSubscription.update(
      customerId = customerId,
      subscriptionId = subscriptionId,
      quantity = Some(quantity),
      prorate = Some(false)
    )

    Try(Await.result(subscription, new DurationInt(10).seconds)) match {
      case TrySuccess(Full(updatedSubscription)) =>
        Full(updatedSubscription)
      
      case TrySuccess(stripeFailure) =>
        logger.error(s"update subscription failed with stipe error: ${stripeFailure}")
        stripeFailure
      
      case TryFail(throwable: Throwable) =>
        logger.error(s"update subscription failed with other error: ${throwable}")
        Failure(throwable.toString)
    }
  }

  def updateCoupon(customerId: String, couponCode: Box[String]) = {
    if (couponCode.isEmpty) {
      Customer.deleteDiscount(customerId)
    } else {
      Customer.update(
        id = customerId,
        coupon = couponCode
      )
    }
  }

  def cancelOpenOrders(oldUser: User) = {
    val openShipments = {
      for {
        subscription <- oldUser.getSubscription.toList
        shipment <- subscription.shipments
        if (shipment.dateShipped.get == null && shipment.shipStationOrderId.get > 0)
      } yield {
        shipment
      }
    }

    openShipments.map(ShipStationService.cancelShipstationOrder(_))
  }

  def removeParent(oldUser: User, fullDelete: Boolean = false) = {
    val user = oldUser.refresh
    val stripeCustomerId = user.map(_.stripeId.get).openOr("")
    
    val subscription = user.flatMap(_.getSubscription)
    val shipments = subscription.map(_.shipments.toList).openOr(Nil)
    val addresses = user.map(_.addresses.toList).openOr(Nil)

    cancelOpenOrders(oldUser)
    val removeCustomer = Customer.delete(stripeCustomerId)

    Try(Await.result(removeCustomer, new DurationInt(10).seconds)) match {
      case TrySuccess(Full(stripeSub)) =>
        if (fullDelete) {
          shipments.map { shipment =>
            shipment.shipmentLineItems.map(_.delete_!)
            shipment.delete_!
          }
          addresses.map(_.delete_!)
          subscription.map(_.delete_!)
          user.map { realUser =>
            realUser.pets.toList.map(_.delete_!)
            realUser.delete_!
          }
        } else {
          user.map(_.cancel)
          shipments.map(_.cancel)
          addresses.map(_.cancel)
          subscription.map(_.cancel)
        }
      case TrySuccess(stripeFailure) =>
        logger.error(s"remove customer failed with stipe error: ${stripeFailure}")
        
        user.map(_.cancel)
        shipments.map(_.cancel)
        addresses.map(_.cancel)
        subscription.map(_.cancel)

        Empty

      case TryFail(throwable: Throwable) =>
        logger.error(s"remove customer failed with other error: ${throwable}")
        
        user.map(_.cancel)
        shipments.map(_.cancel)
        addresses.map(_.cancel)
        subscription.map(_.cancel)

        Empty
    }
  }

  def getStripeCustomer(customerId: String): Box[Customer] = {
    Try(
      Await.result(Customer.get(customerId), new DurationInt(10).seconds)
    ) match {
      case TrySuccess(Full(stripeCustomer)) =>
        Full(stripeCustomer)
      
      case TrySuccess(stripeFailure) =>
        logger.error(s"get customer failed with stripe error: ${stripeFailure}")
        stripeFailure

      case TryFail(throwable: Throwable) =>
        logger.error(s"get customer failed with other error: ${throwable}")
        Empty
    }
  }

  def getStripeCustomerDiscount(customerId: String): Box[Discount] = {
    val stripeCustomer = getStripeCustomer(customerId)

    stripeCustomer match { 
      case Full(customer) => customer.discount

      case Failure(message, _, _) => Failure(message)

      case Empty => Empty
    }
  }

  def getDiscount(customerId: String): Box[Int] = {
    val stripeDiscount = getStripeCustomerDiscount(customerId)

    stripeDiscount match { 
      case Full(discount) =>
        discount.coupon.percentOff

      case Failure(message, _, _) =>
        Failure(message)

      case Empty =>
        Empty
    }
  }

  def getUpcomingInvoice(customerId: String) = {
    Try(
      Await.result(Invoice.getUpcoming(customerId), new DurationInt(10).seconds)
    ) match {
      case TrySuccess(Full(stripeInvoice)) => Full(stripeInvoice)
      
      case TrySuccess(stripeFailure) =>
        logger.error(s"get upcoming invoice failed with stripe error: ${stripeFailure}")
        stripeFailure

      case TryFail(throwable: Throwable) =>
        logger.error(s"get upcoming invoice failed with other error: ${throwable}")
        Empty
    }
  }

  def getInvoice(invoiceId: String) = {
    Try(
      Await.result(Invoice.get(invoiceId), new DurationInt(10).seconds)
    ) match {
      case TrySuccess(Full(stripeInvoice)) => Full(stripeInvoice)
      
      case TrySuccess(stripeFailure) =>
        logger.error(s"get upcoming invoice failed with stripe error: ${stripeFailure}")
        stripeFailure

      case TryFail(throwable: Throwable) =>
        logger.error(s"get upcoming invoice failed with other error: ${throwable}")
        Empty
    }
  }

  def getStripeSubscription(stripeCustomerId: String, subscriptionId: String): Box[StripeSubscription] = {
    Try(
      Await.result(StripeSubscription.get(stripeCustomerId, subscriptionId), new DurationInt(10).seconds)
    ) match {
      case TrySuccess(Full(stripeSubscription)) => Full(stripeSubscription)

      case TrySuccess(stripeFailure) =>
        logger.error(s"get subscription failed with stripe error: ${stripeFailure}")
        stripeFailure

      case TryFail(throwable: Throwable) =>
        logger.error(s"get subscription failed with other error: ${throwable}")
        Empty
    }
  }

  def getCustomerCard(customerId: String): Option[Card] = {
    (for {
      customer <- getStripeCustomer(customerId).toList
      cards <- customer.sources.data
    } yield {
      cards
    }).headOption
  }

  def updateNextShipBillDate(subscription: Subscription, user: Box[User], nextDate: Date): Box[Subscription] = {
    val updatedSubscription = changeStripeBillDate(
      user.map(_.stripeId.get).openOr(""),
      subscription.stripeSubscriptionId.get,
      nextDate.getTime/1000
    )

    updatedSubscription match {
      case Full(stripeSubscription) => 
        Full(subscription.nextShipDate(nextDate).saveMe)
      case _ => Empty
    }
  }

  def updateNextShipDate(subscription: Subscription, user: Box[User]) = {
    val stripeUserId = user.map(_.stripeId.get).openOr("")
    val stripeSubscriptionId = subscription.stripeSubscriptionId.get

    getStripeSubscription(stripeUserId, stripeSubscriptionId) match {
      case Full(stripeSubscription) =>
        val currentPeriodEnd = stripeSubscription.currentPeriodEnd.getOrElse(0l)
        val nextMonthDate = new Date(currentPeriodEnd * 1000L)

        val nextMonthLocaldate = nextMonthDate.toInstant().atZone(ZoneId.of("America/New_York")).toLocalDate()

        val startOfDayDate = nextMonthLocaldate.atStartOfDay(ZoneId.of("America/New_York")).toInstant().toEpochMilli()

        println(s"new date is ${startOfDayDate} =======")

        val updatedSubscription = changeStripeBillDate(
          user.map(_.stripeId.get).openOr(""),
          subscription.stripeSubscriptionId.get,
          startOfDayDate/1000000
        )
          
        subscription.nextShipDate(nextMonthDate).saveMe

      case (_) => Empty
    }
  }

  def changeStripeBillDate(customerId: String, subscriptionId: String, date: Long) = {
    val updatedSubscription = StripeSubscription.update(
      customerId = customerId,
      subscriptionId = subscriptionId,
      trialEnd = Some(date),
      prorate = Some(false)
    )

    //TODO actually use this result or do something, not just yelling into the void
    Try(Await.result(updatedSubscription, new DurationInt(10).seconds)) match {
      case TrySuccess(Full(updatedSubscription)) =>
        Full(updatedSubscription)

      case TrySuccess(stripeFailure) =>
        logger.error(s"update subscription failed with stipe error: ${stripeFailure}")
        stripeFailure

      case TryFail(throwable: Throwable) =>
        logger.error(s"update subscription failed with other error: ${throwable}")
        throwable
    }
  }

  def notTrialSubscription_?(stripeCustomerId: String, subscriptionId: String) = {
    val subscription = getStripeSubscription(stripeCustomerId, subscriptionId)
    val trialStatus = subscription.flatMap(_.status).getOrElse("")
    
    trialStatus != "trialing"
  }

  def parseWhelpDate(whelpDate: String): Box[Date] = {
    val whelpDateFormats = List(
      new java.text.SimpleDateFormat("M/d/y"),
      new java.text.SimpleDateFormat("y-M-d")
    )

    whelpDateFormats.map { dateFormat =>
      tryo(dateFormat.parse(whelpDate))
    }.filter(_.isDefined).headOption.getOrElse(Empty)
  }

  def addNewPet(
    oldUser: User,
    name: String,
    animalType: AnimalType.Value,
    size: AnimalSize.Value,
    product: Product,
    breed: String = "",
    birthday: String = ""
  ): Box[Pet] = {

    val possibleBirthday = parseWhelpDate(birthday)

    val newPet = Pet.createNewPet(
      user = oldUser,
      name = name,
      animalType = animalType,
      size = size,
      product = product,
      whelpDate = possibleBirthday,
      breed = breed
    )

    val updatedSubscription = updateStripeSubscriptionTotal(oldUser)

    updatedSubscription match {
      case Full(stripeSub) => Full(newPet)
      case _ => Empty
    }
  }

  def updateStripeSubscriptionTotal(oldUser: User): Box[StripeSubscription] = {
    val updatedUser = oldUser.refresh
    
    val products: List[Product] = {
      for {
        user <- updatedUser.toList
        pet <- user.activePets
        product <- pet.product.obj
      } yield {
        product
      }
    }

    val (subscriptionId, priceCode) = (
      for {
        user <- updatedUser
        subscription <- user.getSubscription
      } yield {
        (subscription.stripeSubscriptionId.get, subscription.priceCode.get)
      }
    ).openOr(("", ""))

    val prices: List[Double] = products.map { product =>
      Price.getPricesByCode(product, priceCode).map(_.price.get).openOr(0D)
    }

    val totalCost = "%.2f".format(prices.foldLeft(0D)(_ + _)).toDouble

    updateStripeSubscriptionQuantity(
      updatedUser.map(_.stripeId.get).openOr(""),
      subscriptionId,
      tryo((totalCost * 100).toInt).openOr(0)
    )
  }

  def removePet(oldUser: Box[User], pet: Pet): Box[Pet] = {
    oldUser.map(user => removePet(user, pet)).openOr(Empty)
  }

  def removePet(oldUser: User, oldPet: Pet): Box[Pet] = {
    val refreshedPet = Pet.find(By(Pet.petId, oldPet.petId.get))
    val updatedPet = refreshedPet.map(_.status(Status.Cancelled).saveMe)

    val updatedSubscription = updateStripeSubscriptionTotal(oldUser)

    val updatedUser = oldUser.refresh
    
    updatedSubscription match {
      case Full(stripeSub) =>
        if (updatedUser.map(_.activePets.size == 0).openOr(false)) {
          val subscription = updatedUser.flatMap(_.getSubscription)
          subscription.map(_.status(Status.UserSuspended).saveMe)
        }

        updatedPet

      case _ =>
        Empty
    }
  }

  def getGrowthMonthNumber(growthRate: Box[GrowthRate], size: String) = {
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

  def findGrowthMonth(pet: Pet) = {
    val currentDate = LocalDate.now()

    val birthday = tryo(pet.birthday.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate())

    val currentMonth = birthday.map(Period.between(_, currentDate).getMonths).openOr(0)

    val growthDelay = tryo(pet.nextGrowthDelay.get).openOr(0)

    currentMonth - growthDelay
  }

  def checkForNewProduct(pet: Pet, newProduct: Box[Product], user: User) = {
    if (pet.product.obj != newProduct)
      Full((pet, newProduct.map(_.getNameAndSize).openOr(""), user))
    else
      Empty
  }

  def findGrowingPets(subscription: Subscription) = {
    (for {
      user <- subscription.user.obj.toList
      pet <- user.pets.toList
        if (pet.breed.get != null) &&
             (pet.birthday.get != null) &&
             (pet.product.obj.map(_.isZoGuard_?).openOr(false))
    } yield {
      val growthRate = GrowthRate.find(By(GrowthRate.breed, pet.breed.get.toLowerCase))

      val growthMonth = findGrowthMonth(pet)

      growthMonth match {
        case medium 
            if medium == getGrowthMonthNumber(growthRate, "medium") => {
          val newProduct = Product.find(By(Product.size, AnimalSize.DogMediumZo))
          checkForNewProduct(pet, newProduct, user)
        }

        case large 
            if large == getGrowthMonthNumber(growthRate, "large") => {
          val newProduct = Product.find(By(Product.size, AnimalSize.DogLargeZo))
          checkForNewProduct(pet, newProduct, user)
        }

        case xlarge 
            if xlarge == getGrowthMonthNumber(growthRate, "xlarge") => {
          val newProduct = Product.find(By(Product.size, AnimalSize.DogXLargeZo))
          checkForNewProduct(pet, newProduct, user)
        }

        case _ =>
          Empty
      }
    }).flatten
  }

  def updatePuppyProducts(user: User) = {
    for {
      pet <- user.pets.toList
        if (pet.breed.get != null) &&
             (pet.birthday.get != null) &&
             (pet.product.obj.map(_.isZoGuard_?).openOr(false))
    } yield {      
      val growthRate = GrowthRate.find(By(GrowthRate.breed, pet.breed.get.toLowerCase))

      val growthMonth = findGrowthMonth(pet)

      growthMonth match {
        case medium 
            if medium == getGrowthMonthNumber(growthRate, "medium") => {
          val newProduct = Product.find(By(Product.size, AnimalSize.DogMediumZo))
          newProduct.map { product => 
            pet
              .product(product)
              .size(AnimalSize.DogMediumZo)
              .nextGrowthDelay(0)
              .saveMe
          }
        }

        case large 
            if large == getGrowthMonthNumber(growthRate, "large") => {
          val newProduct = Product.find(By(Product.size, AnimalSize.DogLargeZo))
          newProduct.map { product => 
            pet
              .product(product)
              .size(AnimalSize.DogLargeZo)
              .nextGrowthDelay(0)
              .saveMe
          }
        }

        case xlarge 
            if xlarge == getGrowthMonthNumber(growthRate, "xlarge") => {
          val newProduct = Product.find(By(Product.size, AnimalSize.DogXLargeZo))
          newProduct.map { product => 
            pet
              .product(product)
              .size(AnimalSize.DogXLargeZo)
              .nextGrowthDelay(0)
              .saveMe
          }
        }

        case _ =>
      }
    }
  }

  def updateTaxRate(customerId: String, subscriptionId: String, taxRate: Double, email: String) = {
    val updatedSubscription = StripeSubscription.update(
      customerId = customerId,
      subscriptionId = subscriptionId,
      taxPercent = Some(taxRate)
    )

    //TODO actually use this result or do something, not just yelling into the void
    Try(Await.result(updatedSubscription, new DurationInt(10).seconds)) match {
      case TrySuccess(Full(updatedSubscription)) =>
        Full(updatedSubscription)

      case TrySuccess(stripeFailure) =>
        logger.error(s"update subscription tax rate failed with stipe error: ${stripeFailure}")
        stripeFailure

      case TryFail(throwable: Throwable) =>
        logger.error(s"update subscription tax rate failed with other error: ${throwable}")
        throwable
    }
  }
}
