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
import com.mypetdefense.snippet.Plan._
import com.mypetdefense.model._

import java.util.Date

import me.frmr.stripe.{Subscription => StripeSubscription, _}

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
    
    implicit val e = new StripeExecutor("sk_test_vvw6ZOLkVyQWYNEwVocSs27V")

    Customer.create(
      email = Some(email),
      card = Some(stripeToken),
      plan = Some("product")
    )

    val parent = Parent.createNewParent(
      firstName,
      lastName,
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
        petType <- petChoice.is
        petSize <- petSize.is
        petProduct <- petProduct.is
        pet = Pet.createNewPet(
                parent,
                petName,
                petType,
                petSize,
                petProduct
              )
      } yield {
        pet
      })

    println("===================")
    println("pet:")
    println(pet)

    val subscription = (
      for {
        newPlan <- plan.is
        newSubscription = Subscription.createNewSubscription(
          parent,
          newPlan,
          new Date(),
          "",
          new Date()
        )
      } yield {
        newSubscription
      })

    println("===================")
    println("subscription:")
    println(subscription)
  }

  def summary = {
    "#type span *" #> petChoice.is.map(_.toString) &
    "#size span *" #> petSize.is.map(_.toString) &
    "#product span *" #> petProduct.is.map(_.name.get) &
    "#plan span *" #> plan.is.map(_.toString)
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
