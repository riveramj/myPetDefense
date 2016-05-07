package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._

import com.mypetdefense.snippet.PetChoice._
import com.mypetdefense.snippet.PetSize._
import com.mypetdefense.snippet.PetProduct._
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

  def signup() = {
    val selectedPetType = petChoice.is
    val selectedPetSize =  petSize.is
    val selectedPetProduct = petProduct.is

    implicit val e = new StripeExecutor("sk_test_vvw6ZOLkVyQWYNEwVocSs27V")

    val stripeCustomer = Customer.create(
      email = Some(email),
      card = Some(stripeToken)
    )

    for (customer <- stripeCustomer)
      newParentSetup(customer, selectedPetType, selectedPetSize, selectedPetProduct)
  }

  def newParentSetup(
    customer: Box[Customer], 
    selectedPetType: Box[AnimalType.Value],
    selectedPetSize: Box[AnimalSize.Value],
    selectedPetProduct: Box[Product]
  ) = {
    val stripeId = customer.map(_.id).openOr("")
    val parent = Parent.createNewParent(
      firstName,
      lastName,
      stripeId,
      email,
      password,
      phone
    )

    println("===================")
    println("parent:")
    println(parent)

    val shippingAddress = Address.createNewAddress(
      parent,
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
          parent,
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
      parent,
      new Date(),
      new Date()
    )

    println("===================")
    println("subscription:")
    println(subscription)
  }

  def summary = {
    "#type span *" #> petChoice.is.map(_.toString) &
    "#size span *" #> petSize.is.map(_.toString) &
    "#product span *" #> petProduct.is.map(_.name.get)
  }

  def shipping = {
    SHtml.makeFormsAjax andThen
    "#first-name" #> text(firstName, firstName = _) &
    "#last-name" #> text(lastName, lastName = _) &
    "#street-1" #> text(street1, street1 = _) &
    "#street-2" #> text(street2, street2 = _) &
    "#city" #> text(city, city = _) &
    "#state" #> text(state, state = _) &
    "#zip" #> text(zip, zip = _) &
    "#phone" #> text(phone, phone = _) &
    "#email" #> text(email, parentEmail => email = parentEmail.trim) &
    "#password" #> SHtml.password(password, password = _) &
    "#pet-name" #> text(petName, petName = _) &
    "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
    ".checkout" #> ajaxSubmit("Checkout", () => signup)
  }
}
