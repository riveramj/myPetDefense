package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb._
  import http.SHtml._
  import util._
  import util.Helpers._
  import common._
  import util.ClearClearable
  import http._
  import mapper.{By, NullRef, Like}
  import http.js.JsCmds.{Noop, RedirectTo}

import com.mypetdefense.service.PetFlowChoices._
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
  var phone = ""

  var stripeToken = ""
  var couponCode = ""
  var coupon: Box[Coupon] = None

  def validateCouponCode(possileCouponCode: String) = {
    val possibleCoupon = Coupon.find(By(Coupon.couponCode, possileCouponCode.toLowerCase()))

    if (possibleCoupon.isEmpty) {
      S.error("coupon-error", "no match")
    } else {
      couponCode = possileCouponCode
      coupon = possibleCoupon
      S.error("coupon-error", "")
    }
  }

  def signup() = {
    val selectedPetType = petChoice.is
    val selectedPetSize =  petSize.is
    val selectedPetProduct = petProduct.is

    val couponId: String = coupon.map(_.couponCode.get).openOr("")

  
   val stripeCustomer = Customer.create(
     email = Some(email),
     card = Some(stripeToken),
     plan = Some("Product"),
     coupon = Some("oneMonthFree")
   )

   for (customer <- stripeCustomer) 
     newUserSetup(customer, selectedPetType, selectedPetSize, selectedPetProduct)

    RedirectTo(Success.menu.loc.calcDefaultHref)
  }

  def newUserSetup(
    customer: Box[Customer], 
    selectedPetType: Box[AnimalType.Value],
    selectedPetSize: Box[AnimalSize.Value],
    selectedPetProduct: Box[Product]
  ) = {
    val stripeId = customer.map(_.id).openOr("")

    val leads = Lead.findAll(
      By(Lead.email, email),
      NullRef(Lead.parent)
    )
    
    println("===================")
    println("leads:")
    println(leads)

    val referer = leads.headOption.flatMap(_.referer.obj)

    val user = User.createNewUser(
      firstName,
      lastName,
      stripeId,
      email,
      password,
      phone,
      referer,
      UserType.Parent
    )

    leads.map { lead => 
      lead.parent(user).save
    }

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

    EmailActor ! SendWelcomeEmail(email)
  }

  def render = {
    val orderSummary = {
      "#type span *" #> petChoice.is.map(_.toString) &
      "#size span *" #> petSize.is.map(_.toString + " pounds") &
      "#product span *" #> petProduct.is.map(_.name.get)
    }

    SHtml.makeFormsAjax andThen
    orderSummary &
    "#first-name" #> text(firstName, firstName = _) &
    "#last-name" #> text(lastName, lastName = _) &
    "#street-1" #> text(street1, street1 = _) &
    "#street-2" #> text(street2, street2 = _) &
    "#city" #> text(city, city = _) &
    "#state" #> text(state, state = _) &
    "#zip" #> text(zip, zip = _) &
    "#phone" #> text(phone, phone = _) &
    "#email" #> text(email, userEmail => email = userEmail.trim) &
    "#password" #> text(password, userPassword => password = userPassword.trim) &
    "#pet-name" #> text(petName, petName = _) &
    "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
    "#coupon-code" #> ajaxText(couponCode, possibleCode => validateCouponCode(possibleCode)) &
    ".checkout" #> SHtml.ajaxSubmit("Place Order", () => signup)
  }
}
