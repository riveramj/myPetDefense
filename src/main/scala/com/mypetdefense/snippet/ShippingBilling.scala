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

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.Paths._
import com.mypetdefense.actor._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.util.SecurityContext._
import com.mypetdefense.service._
  import ValidationService._

object ShippingBilling extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Shipping and Billing") / "shipping-billing" >>
    loggedIn >>
    parent
}

class ShippingBilling extends Loggable {
  val user = currentUser
  val stripeCustomerId = user.map(_.stripeId.get).openOr("")

  var firstName = ""
  var lastName = ""
  var street1 = ""
  var street2 = ""
  var city = ""
  var state = ""
  var zip = ""
  
  var cardName = ""
  var cardNumber = ""
  var cardExpire = ""
  var stripeToken = ""

  val customerCard = ParentService.getCustomerCard(stripeCustomerId)

  cardNumber = customerCard.map(card => s"Ends in ${card.last4}").getOrElse("")
  cardName = customerCard.flatMap(_.name).getOrElse("")
  cardExpire = customerCard.map(card => s"${card.expMonth}/${card.expYear}").getOrElse("")

  def updateCard() = {
    ParentService.updateStripeCustomerCard(
      stripeCustomerId,
      stripeToken
    )

    S.redirectTo(ShippingBilling.menu.loc.calcDefaultHref)
  }

  def updateAddress() = {
    val validateFields = List(
        checkEmpty(firstName, "#first-name"),
        checkEmpty(lastName, "#last-name"),
        checkEmpty(street1, "#street-1"),
        checkEmpty(city, "#city"),
        checkEmpty(state, "#state"),
        checkEmpty(zip, "#zip")
      ).flatten

    if(validateFields.isEmpty) {
      for {
        user <- user
        shippingAddress <- user.addresses.find(_.addressType == AddressType.Shipping)
      } {
        user
          .firstName(firstName)
          .lastName(lastName)
          .saveMe

          shippingAddress
            .street1(street1)
            .street2(street2)
            .city(city)
            .state(state)
            .zip(zip)
            .saveMe
      }
      S.redirectTo(ShippingBilling.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def render = {
    val dateFormat = new SimpleDateFormat("MMMM dd, yyyy")
    "#page-body-container" #> {
      for {
        user <- user
        shippingAddress <- user.addresses.find(_.addressType == AddressType.Shipping)
        } yield {
          firstName = user.firstName.get
          lastName = user.lastName.get
          street1 = shippingAddress.street1.get
          street2 = shippingAddress.street2.get
        city = shippingAddress.city.get
        state = shippingAddress.state.get
        zip = shippingAddress.zip.get

        SHtml.makeFormsAjax andThen
        "#shipping-billing-nav a [class+]" #> "current" &
        "#user-email *" #> user.email & 
        "#first-name" #> text(firstName, firstName = _) &
        "#last-name" #> text(lastName, lastName = _) &
        "#street-1" #> text(street1, street1 = _) &
        "#street-2" #> text(street2, street2 = _) &
        "#city" #> text(city, city = _) &
        "#state" #> text(state, state = _) &
        "#zip" #> text(zip, zip = _) &
        "#cardholder-name" #> text(cardName, cardName = _) &
        "#old-card-last4" #> hidden(cardNumber = _, cardNumber) &
        "#card-expiry" #> text(cardExpire, cardExpire = _) &
        "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
        ".update-billing" #> SHtml.ajaxSubmit("Update Card", updateCard) &
        ".save-changes" #> SHtml.ajaxSubmit("Update Address", updateAddress)
      }
    }
  }
}
