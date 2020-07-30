package com.mypetdefense.snippet.signup

import java.util.Date

import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.service.PetFlowChoices._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import com.mypetdefense.snippet.MyPetDefenseEvent
import com.mypetdefense.util.{ClearNodesIf, SecurityContext}
import me.frmr.stripe.{Customer, StripeExecutor}
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._
import net.liftweb.util._

import scala.collection.mutable.LinkedHashMap
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure => TryFail, Success => TrySuccess, _}

object Checkout extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu = Menu.i("Checkout") / "checkout" >>
    completedPet >>
    createdAccount
}

case class PromoCodeMessage(status: String) extends MyPetDefenseEvent("promotion-code-message")

class Checkout extends Loggable {
  val stripeSecretKey = Props.get("secret.key") openOr ""
  implicit val e = new StripeExecutor(stripeSecretKey)

  val currentUser = SecurityContext.currentUser
  val email = currentUser.map(_.email.get).openOr("")

  var firstName = currentUser.map(_.firstName.get).getOrElse("")
  var lastName = currentUser.map(_.lastName.get).getOrElse("")
  var street1 = ""
  var street2 = ""
  var city = ""
  var state = ""
  var zip = ""
  var taxRate = 0D
  var taxDue = 0D
  var priceAdditionsRenderer: Box[IdMemoizeTransform] = None

  var stripeToken = ""
  var coupon: Box[Coupon] = Empty
  var couponCode = coupon.map(_.couponCode.get).openOr("")

  val pets = completedPets.is
  println(pets.values.map(_.size))
  println(AnimalSize.DogSmallZo)
  println(AnimalSize.DogXLargeZo)
  println("AnimalSize.DogXLargeZo")
  val petCount = pets.size

  val smMedPets = pets.values.count { pet =>
    pet.size.get != AnimalSize.DogLargeZo && pet.size.get != AnimalSize.DogXLargeZo
  }

  println("smMedPets")
  println(smMedPets)
  println("smMedPets")

  val lgXlPets = petCount - smMedPets
  
  val subtotal = (smMedPets * 24.99) + (lgXlPets * 27.99)
  val discount = petCount match {
    case 0 | 1 => 0
    case _ => subtotal * 0.1
  }
  val promotionAmount = coupon.map(_.percentOff.get).openOr(0).toDouble
  val subtotalWithDiscount = subtotal - discount - (subtotal * (promotionAmount/100))

  val pennyCount = (subtotal * 100).toInt

  def validateCouponCode() = {
    val possibleCoupon = Coupon.find(By(Coupon.couponCode, couponCode.toLowerCase()))

    if (possibleCoupon.isEmpty) {
      PromoCodeMessage("error")
    } else {
      coupon = possibleCoupon
      PetFlowChoices.coupon(coupon)

      (
        PromoCodeMessage("success") &
        priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
      )
    }
  }

  def calculateTax(possibleState: String, possibleZip: String) = {
    state = possibleState
    zip = possibleZip

    val taxInfo = TaxJarService.findTaxAmoutAndRate(
      city,
      state,
      zip,
      subtotalWithDiscount
    )

    taxDue = taxInfo._1
    taxRate = taxInfo._2

    priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
  }

  def signup() = {
    val validateFields = List(
        checkEmpty(firstName, "#first-name"),
        checkEmpty(lastName, "#last-name"),
        checkEmpty(street1, "#street-1"),
        checkEmpty(city, "#city"),
        checkEmpty(state, "#state"),
        checkEmpty(zip, "#zip")
      ).flatten

    if(validateFields.isEmpty) {
      (coupon, petCount) match {
        case (Full(_), _) =>
        case (Empty, 2) => 
          coupon = Coupon.find(By(Coupon.couponCode, "multiPet"))
        case (_, _) => 
      }

      val couponId = coupon.map(_.couponCode.get)

      val stripeCustomer = {
        if (couponId.isEmpty) {
          Customer.create(
            email = Some(email),
            card = Some(stripeToken),
            plan = Some("pennyProduct"),
            quantity = Some(pennyCount),
            taxPercent = Some(taxRate)
          )
        } else {
          Customer.create(
            email = Some(email),
            card = Some(stripeToken),
            plan = Some("pennyProduct"),
            quantity = Some(pennyCount),
            taxPercent = Some(taxRate),
            coupon = couponId
          )
        }
      }

      Try(Await.result(stripeCustomer, new DurationInt(7).seconds)) match {
        case TrySuccess(Full(customer)) =>
          val user = newUserSetup(
            customer
          )

          PetFlowChoices.petCount(Full(petCount))

          PetFlowChoices.completedPets(LinkedHashMap.empty)

          val total = subtotalWithDiscount + taxDue
          
          PetFlowChoices.total(Full(total))
          
          PetFlowChoices.freeMonths(coupon.map(_.freeMonths.get))

          S.redirectTo(Success.menu.loc.calcDefaultHref)

        case TrySuccess(stripeFailure) =>
          logger.error("create customer failed with: " + stripeFailure)
          Alert(s"An error has occurred ${stripeFailure}. Please Try again.")

        case TryFail(throwable: Throwable) =>
          logger.error("create customer failed with: " + throwable)
          Alert("An error has occured. Please try again.")
      }
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def createNewPets(user: Box[User]) = {
    for {
      usr <- user.toList
      pet <- pets.values
    } yield {
      Pet.createNewPet(pet, usr)
    }
  }

  def newUserSetup(customer: Customer) = {
    val stripeId = customer.id

    val user = currentUser.map { oldUser =>
      oldUser
        .firstName(firstName)
        .lastName(lastName)
        .stripeId(stripeId)
        .coupon(coupon)
        .referer(coupon.flatMap(_.agency.obj))
        .saveMe
    }

    Address.createNewAddress(
      user,
      street1,
      street2,
      city,
      state,
      zip,
      AddressType.Shipping
    )
    
    val pets = createNewPets(user)

    val subscriptionId = (
      for {
        rawSubscriptions <- customer.subscriptions
        subscription <- rawSubscriptions.data.headOption
      } yield {
        subscription.id
      }).flatten.getOrElse("")

    val mpdSubscription = Subscription.createNewSubscription(
      user,
      subscriptionId,
      new Date(),
      new Date(),
      priceCode.is.openOr(Price.defaultPriceCode)
    )

    pets.map(SubscriptionBox.createNewBox(mpdSubscription, _))

    TaggedItem.createNewTaggedItem(
      subscription = Full(mpdSubscription),
      tag = Tag.useBox
    )

    if (Props.mode == Props.RunModes.Production) {
      EmailActor ! NewSaleEmail(user, petCount, coupon.map(_.couponCode.get).openOr(""))
    }

    EmailActor ! SendWelcomeEmail(user)
    
    user
  }

  def render = {
    val orderSummary = {
      "#order-summary" #> SHtml.idMemoize { renderer =>
        priceAdditionsRenderer = Full(renderer)

        val monthlyTotal = subtotalWithDiscount + taxDue

        "#subtotal span *" #> f"$$$subtotal%2.2f" &
        "#discount" #> ClearNodesIf(discount == 0) &
        "#promotion" #> ClearNodesIf(promotionAmount == 0) &
        "#promotion span *" #> f"$$$promotionAmount%2.2f" &
        "#discount span *" #> f"$$$discount%2.2f" &
        "#tax" #> ClearNodesIf(taxDue == 0D) &
        "#tax span *" #> f"$$$taxDue%2.2f" &
        "#monthly-total span *" #> f"$$$monthlyTotal%2.2f" &
        {
          val freeMonths = coupon.map(_.freeMonths.get).openOr(0)

          if(coupon.isEmpty || freeMonths == 0) {
            "#order span *" #> f"$$$monthlyTotal%2.2f"
          } else {
            "#order span *" #> {
              if (freeMonths == 1) {
                s"First Month Free"
              } else {
                s"First ${freeMonths} months free"
              }
            }
          }
        }
      }
    }

    val successCoupon = {
      if (!coupon.isEmpty)
        "promo-success"
      else
        ""
    }

    SHtml.makeFormsAjax andThen
    orderSummary &
    "#first-name" #> text(firstName, firstName = _) &
    "#last-name" #> text(lastName, lastName = _) &
    "#street-1" #> text(street1, street1 = _) &
    "#street-2" #> text(street2, street2 = _) &
    "#city" #> ajaxText(city, city = _) &
    "#state" #> ajaxText(state, possibleState => calculateTax(possibleState, zip)) &
    "#zip" #> ajaxText(zip, possibleZip => calculateTax(state, possibleZip)) &
    "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
    ".checkout" #> SHtml.ajaxSubmit("Place Order", () => signup) &
    ".agreement" #> ClearNodesIf(subtotalWithDiscount == 0) &
    ".promotion-info [class+]" #> successCoupon &
    "#promo-code" #> ajaxText(couponCode, couponCode = _) &
    ".apply-promo [onClick]" #> SHtml.ajaxInvoke(() =>
      validateCouponCode())
  }
}
