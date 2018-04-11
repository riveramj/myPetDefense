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

  val menuBilling = Menu.i("Billing Renew") / "billing" >>
    loggedIn >>
    parent
}

class ShippingBilling extends Loggable {
  val user = currentUser.flatMap(_.refresh)
  val stripeCustomerId = user.map(_.stripeId.get).openOr("")

  var firstName = ""
  var lastName = ""
  var street1 = ""
  var street2 = ""
  var city = ""
  var state = ""
  var zip = ""
  
  var cardNumberLastFour = ""
  var stripeToken = ""
  var promoCode = ""

  val customerCard = ParentService.getCustomerCard(stripeCustomerId)

  cardNumberLastFour = customerCard.map(card => card.last4.toString).getOrElse("Not Found")

  def updateCard(parent: User) = {
    ParentService.updateStripeCustomerCard(
      stripeCustomerId,
      stripeToken,
      parent
    )
  }

  def updateBilling(parent: User)() = {
    updateCard(parent)

    if (Props.mode != Props.RunModes.Pilot) {
      EmailActor ! BillingUpdatedEmail(parent)
    }

    Thread.sleep(3000)
    S.redirectTo(ShippingBilling.menu.loc.calcDefaultHref)
  }

  def updateBillingAddWinterCoupon(parent: User)() = {
    updateCard(parent)

    val coupon = Coupon.find(By(Coupon.couponCode,"winter17"))
    parent.coupon(coupon).saveMe

    ParentService.updateCoupon(parent.stripeId.get, coupon.map(_.couponCode.get))

    if (Props.mode != Props.RunModes.Pilot) {
      EmailActor ! BillingUpdatedEmail(parent)
    }

    S.redirectTo(billingThanksPage.loc.calcDefaultHref)
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
        "#old-card-last4 span *" #> cardNumberLastFour &
        "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
        ".update-billing" #> SHtml.ajaxSubmit("Update Card", updateBilling(user) _) &
        ".save-changes" #> SHtml.ajaxSubmit("Update Address", updateAddress _)
      }
    }
  }

  def billingRenew = {
    val dateFormat = new SimpleDateFormat("MMMM dd, yyyy")
    "#page-body-container" #> {
      for {
        user <- user
      } yield {
        firstName = user.firstName.get
        lastName = user.lastName.get

        SHtml.makeFormsAjax andThen
        "#shipping-billing-nav a [class+]" #> "current" &
        "#user-email *" #> user.email & 
        "#first-name" #> text(firstName, firstName = _) &
        "#last-name" #> text(lastName, lastName = _) &
        "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
        "#promo-code" #> text(promoCode, promoCode = _) &
        ".update-billing" #> SHtml.ajaxSubmit("Update Card", updateBillingAddWinterCoupon(user) _)
      }
    }
  }
}
