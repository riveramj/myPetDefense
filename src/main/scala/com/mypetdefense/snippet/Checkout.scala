package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
import net.liftweb.mapper.{By, NullRef}

import com.mypetdefense.service.PetFlowChoices._
import com.mypetdefense.model._

import java.util.Date
import java.time.MonthDay

import me.frmr.stripe.{Subscription => StripeSubscription, _}

import dispatch._, Defaults._

object Checkout extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Checkout") / "checkout"
}

class Checkout extends Loggable {

  var email = ""
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

  def signup() = {
    val selectedPetType = petChoice.is
    val selectedPetSize =  petSize.is
    val selectedPetProduct = petProduct.is

    implicit val e = new StripeExecutor("sk_test_vvw6ZOLkVyQWYNEwVocSs27V")

    val stripeCustomer = Customer.create(
      email = Some(email),
      card = Some(stripeToken),
      plan = Some("product")
    )

    for (customer <- stripeCustomer)
      newUserSetup(customer, selectedPetType, selectedPetSize, selectedPetProduct)
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
    "#pet-name" #> text(petName, petName = _) &
    "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
    ".checkout" #> ajaxSubmit("Checkout", () => signup)
  }
}
