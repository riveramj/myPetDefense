package com.fleaTick.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._

import com.fleaTick.snippet.PetChoice._
import com.fleaTick.snippet.PetSize._
import com.fleaTick.snippet.PetProduct._
import com.fleaTick.snippet.Plan._
import com.fleaTick.model._

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

  var cardNumber = ""
  var cvc = ""
  var expire = ""

  def signupUser() = {
    val user = User.createNewUser(
      firstName,
      lastName,
      email,
      password,
      street1,
      street2,
      city,
      state,
      zip,
      phone
    )

    println("===================")
    println("user:")
    println(user)

    val pet = (
      for {
        petType <- petChoice.is
        petSize <- petSize.is
        pet = Pet.createNewPet(
                user,
                petName,
                petType,
                petSize
              )
      } yield {
        pet
      })

    println("===================")
    println("pet:")
    println(pet)

    val subscription = (
      for {
        newPet <- pet
        newPlan <- plan.is
        newSubscription = Subscription.createNewSubscription(
          user,
          newPet,
          newPlan
        )
      } yield {
        newSubscription
      })

    println("===================")
    println("subscription:")
    println(subscription)

    val order = (
      for {
        newSubscription <- subscription
        newOrder = Order.createNewOrder(
          user,
          newSubscription,
          List(),
          ""
        )
      } yield {
        newOrder
      })

    println("===================")
    println("order:")
    println(order)
    println("===================")
 }

  def summary = {
    "#type span *" #> petChoice.is.map(_.toString) &
    "#size span *" #> petSize.is.map(_.toString) &
    "#product span *" #> petProduct.is &
    "#plan span *" #> plan.is.map(_.toString)
  }

  def shipping = {
    "#first-name" #> SHtml.text(firstName, firstName = _) &
    "#last-name" #> SHtml.text(lastName, lastName = _) &
    "#street-1" #> SHtml.text(street1, street1 = _) &
    "#street-2" #> SHtml.text(street2, street2 = _) &
    "#city" #> SHtml.text(city, city = _) &
    "#state" #> SHtml.text(state, state = _) &
    "#zip" #> SHtml.text(zip, zip = _) &
    "#phone" #> SHtml.text(phone, phone = _) &
    "#card-number" #> SHtml.text(cardNumber, cardNumber = _) &
    "#cvc" #> SHtml.text(cvc, cvc = _) &
    "#expire" #> SHtml.text(expire, expire = _) &
    "#email" #> SHtml.text(email, userEmail => email = userEmail.trim) &
    "#password" #> SHtml.password(password, password = _) &
    "#pet-name" #> SHtml.text(petName, petName = _) &
    ".signup button" #> SHtml.onSubmitUnit(signupUser)
  }
}
