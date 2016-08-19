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

class Checkout extends Loggable {
  val stripeSecretKey = Props.get("secret.key") openOr ""
  implicit val e = new StripeExecutor(stripeSecretKey)

  var email = ""
  var password = ""
  var petName = ""
  
  var firstName = ""
  var lastName = ""
  var street1 = ""
  var street2 = ""
  var city = ""
  var state = ""
  var zip = ""
  var taxDue = 0D
  var priceAdditionsRenderer: Box[IdMemoizeTransform] = None

  var stripeToken = ""
  var couponCode = ""
  var coupon: Box[Coupon] = None

  def validateCouponCode() = {
    val possibleCoupon = Coupon.find(By(Coupon.couponCode, couponCode.toLowerCase()))

    if (possibleCoupon.isEmpty) {
      ValidationError("promo-code", "Did not find that code. Please try again.")
    } else {
      coupon = possibleCoupon
      priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
    }
  }

  def calculateTax(possibleZip: String) = {
    zip = possibleZip

    if ((zip.length() > 4) && (state.toLowerCase() == "ga")) {
      taxDue = TaxJarService.findTaxAmout(state, zip, "9.99")
      println(taxDue + " is due")
    } else {
      taxDue = 0D
    }

    priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
  }

  def signup() = {
    val selectedPetType = petChoice.is
    val selectedPetSize =  petSize.is
    val selectedPetProduct = petProduct.is

    val validateFields = List(
        checkEmail(email, "#email"),
        checkEmpty(petName, "#pet-name"),
        checkEmpty(firstName, "#first-name"),
        checkEmpty(lastName, "#last-name"),
        checkEmpty(password, "#password"),
        checkEmpty(street1, "#street-1"),
        checkEmpty(city, "#city"),
        checkEmpty(state, "#state"),
        checkEmpty(zip, "#zip")
      ).flatten

    if(validateFields.isEmpty) {
      val couponId: String = coupon.map(_.couponCode.get).openOr("")

      val stripeCustomer = Customer.create(
        email = Some(email),
        card = Some(stripeToken),
        plan = Some("Product"),
        coupon = Some(couponId)
      )

      for (customer <- stripeCustomer) {
        newUserSetup(
          customer, 
          selectedPetType, 
          selectedPetSize, 
          selectedPetProduct
        )
      }

      S.redirectTo(Success.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def newUserSetup(
    customer: Box[Customer], 
    selectedPetType: Box[AnimalType.Value],
    selectedPetSize: Box[AnimalSize.Value],
    selectedPetProduct: Box[Product]
  ) = {
    val stripeId = customer.map(_.id).openOr("")

    val user = User.createNewUser(
      firstName,
      lastName,
      stripeId,
      email,
      password,
      "",
      coupon.flatMap(_.referer.obj),
      UserType.Parent
    )

    println("===================")
    println("user:")
    println(user)

    val shippingAddress = Address.createNewAddress(
      Full(user),
      None,
      street1,
      street2,
      city,
      state,
      zip,
      AddressType.Shipping
    )

    println("===================")
    println("shippingAddress:")
    println(shippingAddress)

    val pet = (
      for {
        petType <- selectedPetType
        petSize <- selectedPetSize
        petProduct <- selectedPetProduct
      } yield {
        Pet.createNewPet(
          user,
          petName,
          petType,
          petSize,
          petProduct
        )
    })

    println("===================")
    println("pet:")
    println(pet)

    val subscription = Subscription.createNewSubscription(
      user,
      new Date(),
      new Date()
    )

    println("===================")
    println("subscription:")
    println(subscription)

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

        "#price-additions" #> {
          "#tax" #> ClearNodesIf(taxDue == 0D) &
          "#promo-discount" #> ClearNodesIf(coupon.isEmpty) &
          "#tax #tax-amount" #> f"$taxDue%2.2f"
        } &
        {
          if(coupon.isEmpty) {
            "#order-total .monthly-charge .amount *" #> "$18.98"
          } else {
            "#order-total .monthly-charge *" #> s"${coupon.map(_.freeMonths).openOr(0)} free!"
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
    "#city" #> text(city, city = _) &
    "#state" #> ajaxText(state, state = _) &
    "#zip" #> ajaxText(zip, possibleZip => calculateTax(possibleZip)) &
    "#email" #> text(email, userEmail => email = userEmail.trim) &
    "#password" #> SHtml.password(password, userPassword => password = userPassword.trim) &
    "#pet-name" #> text(petName, petName = _) &
    "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
    "#promo-code" #> ajaxText(couponCode, couponCode = _) &
    "#apply-promo [onClick]" #> SHtml.ajaxInvoke(() => validateCouponCode()) &
    ".checkout" #> SHtml.ajaxSubmit("Place Order", () => signup)
  }
}
