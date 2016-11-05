package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb._
  import http.SHtml._
  import util._
  import util.Helpers._
  import common._
  import util.ClearClearable
  import http._
  import mapper.{By, NullRef}
  import js._
      import JsCmds._

import com.mypetdefense.service._
  import ValidationService._
  import PetFlowChoices._

import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.model._
import com.mypetdefense.actor._

import java.util.Date
import java.time.MonthDay

import scala.util.{Failure => TryFail, Success => TrySuccess, _}

import scala.concurrent.Await
import scala.concurrent.duration._

import me.frmr.stripe.{StripeExecutor, Customer, Coupon => StripeCoupon}

import dispatch._, Defaults._

object Checkout extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Checkout") / "checkout" >>
    petChosen >>
    productChosen >>
    sizeChosen
}

case class PromoCodeMessage(status: String) extends MyPetDefenseEvent("promotion-code-message")

class Checkout extends Loggable {
  val stripeSecretKey = Props.get("secret.key") openOr ""
  implicit val e = new StripeExecutor(stripeSecretKey)

  var email = ""
  var password = ""
  
  var firstName = ""
  var cardholderName = ""
  var lastName = ""
  var street1 = ""
  var street2 = ""
  var city = ""
  var state = ""
  var zip = ""
  var taxRate = 0D
  var taxDue = 0D
  var priceAdditionsRenderer: Box[IdMemoizeTransform] = None

  var stripeToken = ""
  var couponCode = ""
  var coupon: Box[Coupon] = None

  def validateCouponCode() = {
    val possibleCoupon = Coupon.find(By(Coupon.couponCode, couponCode.toLowerCase()))

    if (possibleCoupon.isEmpty) {
      coupon = None
      
      (
        PromoCodeMessage("error") &
        priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
      )
    } else {
      coupon = possibleCoupon

      (
        PromoCodeMessage("success") &
        priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
      )
    }
  }

  def calculateTax(possibleState: String, possibleZip: String) = {
    state = possibleState
    zip = possibleZip

    if ((zip.length() > 4) && (state.toLowerCase() == "ga")) {
      val taxInfo = TaxJarService.findTaxAmoutAndRate(
        city,
        state,
        zip,
        9.99
      )

      taxDue = taxInfo._1
      taxRate = taxInfo._2
    } else {
      taxDue = 0D
      taxRate = 0D
    }

    priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
  }

  def signup() = {
    val selectedPetType = petChoice.is
    val selectedPetSize =  petSize.is
    val selectedPetProduct = petProduct.is

    val validateFields = List(
        checkEmail(email, "#email"),
        checkEmpty(firstName, "#first-name"),
        checkEmpty(cardholderName, "#cardholder-name"),
        checkEmpty(lastName, "#last-name"),
        checkEmpty(password, "#password"),
        checkEmpty(street1, "#street-1"),
        checkEmpty(city, "#city"),
        checkEmpty(state, "#state"),
        checkEmpty(zip, "#zip")
      ).flatten

    if(validateFields.isEmpty) {
      val couponId = coupon.map(_.couponCode.get)

      val stripeCustomer = {
        if (couponId.isEmpty) {
          Customer.create(
            email = Some(email),
            card = Some(stripeToken),
            plan = Some("Product"),
            taxPercent = Some(taxRate)
          )
        } else {
          Customer.create(
            email = Some(email),
            card = Some(stripeToken),
            plan = Some("Product"),
            taxPercent = Some(taxDue),
            coupon = couponId
          )
        }
      }

      Try(Await.result(stripeCustomer, new DurationInt(3).seconds)) match {
        case TrySuccess(Full(customer)) =>
          newUserSetup(
            customer, 
            petName.is,
            selectedPetType, 
            selectedPetSize, 
            selectedPetProduct
          )

          val total = 9.99D + taxDue
          PetFlowChoices.total(Full(total))

          S.redirectTo(Success.menu.loc.calcDefaultHref)

        case TrySuccess(stripeFailure) =>
          logger.error("create customer failed with: " + stripeFailure)
          Alert("An error has occured. Please try again.")
        
        case TryFail(throwable: Throwable) =>
          logger.error("create customer failed with: " + throwable)
          Alert("An error has occured. Please try again.")
      }
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def newUserSetup(
    customer: Customer, 
    petName: Box[String],
    selectedPetType: Box[AnimalType.Value],
    selectedPetSize: Box[AnimalSize.Value],
    selectedPetProduct: Box[Product]
  ) = {
    val stripeId = customer.id

    val user = User.createNewUser(
      firstName,
      lastName,
      stripeId,
      email,
      password,
      "",
      coupon,
      coupon.flatMap(_.agency.obj),
      None,
      UserType.Parent
    )

    Address.createNewAddress(
      Full(user),
      street1,
      street2,
      city,
      state,
      zip,
      AddressType.Shipping
    )

    for {
      petType <- selectedPetType
      petSize <- selectedPetSize
      petProduct <- selectedPetProduct
      petName <- petName
    } yield {
      Pet.createNewPet(
        user,
        petName,
        petType,
        petSize,
        petProduct
      )
    }


    println(customer.subscriptions + " sub")

    val subscriptionId = (
      for {
        rawSubscriptions <- customer.subscriptions
        subscription <- rawSubscriptions.data.headOption
      } yield {
        println(subscription.id + " is the id?")
        subscription.id
      }).flatMap(identity).getOrElse("")

    Subscription.createNewSubscription(
      user,
      subscriptionId,
      new Date(),
      new Date()
    )

    if (Props.mode == Props.RunModes.Production) {
      EmailActor ! NewSaleEmail()
    }

    EmailActor ! SendWelcomeEmail(user)
  }

  def render = {
    val orderSummary = {
      "#type span *" #> petChoice.is.map(_.toString) &
      "#size span *" #> petSize.is.map(_.toString + " pounds") &
      "#product span *" #> petProduct.is.map(_.name.get)
    }

    val orderTotal = {
      "#order" #> SHtml.idMemoize { renderer =>
        priceAdditionsRenderer = Full(renderer)

        val total = 9.99 + taxDue

        "#price-additions" #> ClearNodesIf((taxDue == 0D) && (coupon.isEmpty)) &
        "#price-additions" #> {
          "#tax" #> ClearNodesIf(taxDue == 0D) &
          "#promo-discount" #> ClearNodesIf(coupon.isEmpty) &
          "#promo-discount-note" #> ClearNodesIf(coupon.isEmpty) &
          "#tax #tax-amount" #> f"$taxDue%2.2f"
        } &
        {
          if(coupon.isEmpty) {
            "#order-total h3 [class!]" #> "promo" &
            "#order-total .monthly-charge [class!]" #> "promo" &
            "#order-total .monthly-charge .amount *" #> f"$$$total%2.2f"
          } else {
            "#order-total h3 [class+]" #> "promo" &
            "#order-total .monthly-charge [class+]" #> "promo" &
            "#order-total .monthly-charge *" #> {
              val freeMonths = coupon.map(_.freeMonths).openOr("0")
              if (freeMonths == 1) {
                s"FREE for first ${freeMonths} month"
              } else {
                s"FREE for first ${freeMonths} months"
              }
            }
          }
        }
      }
    }

    SHtml.makeFormsAjax andThen
    orderSummary &
    orderTotal &
    "#first-name" #> text(firstName, firstName = _) &
    "#last-name" #> text(lastName, lastName = _) &
    "#street-1" #> text(street1, street1 = _) &
    "#street-2" #> text(street2, street2 = _) &
    "#city" #> ajaxText(city, city = _) &
    "#state" #> ajaxText(state, possibleState => calculateTax(possibleState, zip)) &
    "#zip" #> ajaxText(zip, possibleZip => calculateTax(state, possibleZip)) &
    "#email" #> text(email, userEmail => email = userEmail.trim) &
    "#password" #> SHtml.password(password, userPassword => password = userPassword.trim) &
    "#cardholder-name" #> text(cardholderName, cardholderName = _) &
    "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
    "#promo-code" #> ajaxText(couponCode, couponCode = _) &
    ".apply-promo [onClick]" #> SHtml.ajaxInvoke(() => validateCouponCode()) &
    ".checkout" #> SHtml.ajaxSubmit("Place Order", () => signup)
  }
}
