package com.mypetdefense.snippet 

import net.liftweb._
  import http.SHtml._
  import util._
    import Helpers._
  import http._
  import common._
  import sitemap.Menu
  import js._
      import JsCmds._

import com.mypetdefense.service._
    import ValidationService._
    import PetFlowChoices._

import com.mypetdefense._
  import model._
  import snippet.admin.Dashboard
  import snippet.agency.AgencyOverview
import com.mypetdefense.util.{SecurityContext, ClearNodesIf}
import com.mypetdefense.actor._

import me.frmr.stripe.{StripeExecutor, Customer, Coupon => StripeCoupon, Subscription => StripeSubscription}

object BoxCheckout extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Box Checkout") / "box-checkout"
}

case object UseNewCard extends MyPetDefenseEvent("use-new-card")

class BoxCheckout extends Loggable {
  import BoxCheckout._

  var user = BoxDetails.thanksgivingBoxMenu.currentValue
  var existingUser_? =  if (user.isDefined) true else false
  var useExistingCard = true

  var email = user.map(_.email.get).openOr("")
  var firstName = user.map(_.firstName.get).openOr("")
  var lastName = user.map(_.lastName.get).openOr("")
  var address = user.flatMap(_.shippingAddress)
  var street1 = address.map(_.street1.get).openOr("")
  var street2 = address.map(_.street2.get).openOr("")
  var city = address.map(_.city.get).openOr("")
  var state = address.map(_.state.get).openOr("")
  var zip = address.map(_.zip.get).openOr("")

  var password = ""

  var taxDue = 0D
  var taxRate = 0D
  var priceAdditionsRenderer: Box[IdMemoizeTransform] = None
  var billingCardRenderer: Box[IdMemoizeTransform] = None
  var checkoutRenderer: Box[IdMemoizeTransform] = None

  var stripeToken = ""

  user.map(SecurityContext.logIn(_))

  def findSubtotal = {
    val cart = BoxDetailsFlow.shoppingCart.is
    
    cart.map { case (product, quantity) =>
      product.price.get * quantity
    }.foldLeft(0D)(_+_)
  }

  def calculateTax(possibleState: String, possibleZip: String) = {
    state = possibleState
    zip = possibleZip

    val taxInfo = TaxJarService.findTaxAmoutAndRate(
      city,
      state,
      zip,
      findSubtotal
    )

    taxDue = taxInfo._1
    taxRate = taxInfo._2

    priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
  }

  def orderBox() = {
    val validateFields = List(
        validEmailFormat(email, "#email"),
        checkEmpty(firstName, "#first-name"),
        checkEmpty(lastName, "#last-name"),
        checkEmpty(street1, "#street-1"),
        checkEmpty(city, "#city"),
        checkEmpty(state, "#state"),
        checkEmpty(zip, "#zip")
      ).flatten

    if(!validateFields.isEmpty) {
      validateFields.foldLeft(Noop)(_ & _)
    } else {
      val stripeId = user.map(_.stripeId.get).openOr("")
      val amountPaid = findSubtotal + taxDue

      val boxCharge = {
        val stripeAmount = (amountPaid * 100).toLong
        val internalSaleDescription = "Thanksgiving Box"

        if (existingUser_? && useExistingCard) {
          println("in if")
          val stripeCustomer = ParentService.getStripeCustomer(stripeId)

          ParentService.chargeStripeCustomer(
            stripeAmount,
            stripeCustomer.map(_.id),
            internalSaleDescription
          )
        } else if (existingUser_?) {
          println("in if else")
          val stripeCustomer = ParentService.getStripeCustomer(stripeId)

          ParentService.chargeStripeCustomerNewCard(
            stripeAmount,
            stripeCustomer.map(_.id),
            stripeToken,
            internalSaleDescription
          )
        } else {
          println("in else")
          ParentService.chargeGuestCard(
            stripeAmount,
            stripeToken,
            internalSaleDescription
          )
        }
      }

      (for {
        charge <- boxCharge
      } yield {
        val address = Address.create
          .street1(street1)
          .street2(street2)
          .city(city)
          .state(state.toUpperCase)
          .zip(zip)

        val realEmail = {
          if (existingUser_?)
            user.map(_.email.get).openOr("")
          else
            email
        }

        val cart = BoxDetailsFlow.shoppingCart.is

        val newBoxOrder = BoxOrder.createBoxOrder(
          user,
          firstName,
          lastName,
          realEmail,
          address,
          charge.id.getOrElse(""),
          amountPaid,
          taxDue,
          cart.toList
        )

        EmailActor ! BoxReceiptEmail(newBoxOrder)

        BoxDetailsFlow.boxSale(Full(amountPaid, cart))

        S.redirectTo(Success.menu.loc.calcDefaultHref)
      }).openOr(Noop)
    }
  }

  def useNewCard() = {
    useExistingCard = false

    UseNewCard
  }

  def login = {
    val loginResult = LoginService.login(email, password, true)

    if (loginResult == Noop) {
      println("true")
      checkoutRenderer.map(_.setHtml).openOr(Noop)
    } else {
      println("false")
      loginResult
    }
  }

  val loginBindings = {
    "#login-container" #> {
      "#email" #> SHtml.text(email, email = _) &
      "#password" #> SHtml.password(password, password = _) &
      "#login" #> SHtml.ajaxSubmit("Log In", () => login)
    }
  }

  def render = {
    val orderSummary = {
      "#order-summary" #> SHtml.idMemoize { renderer =>
        priceAdditionsRenderer = Full(renderer)

        val subtotal = findSubtotal

        val total = subtotal + taxDue

        ".subtotal-amount *" #> f"$$$subtotal%2.2f" &
        "#tax" #> ClearNodesIf(taxDue == 0D) &
        ".tax-amount *" #> f"$$$taxDue%2.2f" &
        ".order-amount *" #> f"$$$total%2.2f"
      }
    }

    SHtml.makeFormsAjax andThen
    loginBindings &
    ".checkout-container" #> SHtml.idMemoize { renderer =>
      user = SecurityContext.currentUser
      existingUser_? =  if (user.isDefined) true else false
      useExistingCard = true

      email = user.map(_.email.get).openOr("")
      firstName = user.map(_.firstName.get).openOr("")
      lastName = user.map(_.lastName.get).openOr("")
      address = user.flatMap(_.shippingAddress)
      street1 = address.map(_.street1.get).openOr("")
      street2 = address.map(_.street2.get).openOr("")
      city = address.map(_.city.get).openOr("")
      state = address.map(_.state.get).openOr("")
      zip = address.map(_.zip.get).openOr("")

      checkoutRenderer = Full(renderer)
      orderSummary &
      "#first-name" #> text(firstName, firstName = _) &
      "#last-name" #> text(lastName, lastName = _) &
      "#street-1" #> text(street1, street1 = _) &
      "#street-2" #> text(street2, street2 = _) &
      "#city" #> ajaxText(city, city = _) &
      "#state" #> ajaxText(state, possibleState => calculateTax(possibleState, zip)) &
      "#zip" #> ajaxText(zip, possibleZip => calculateTax(state, possibleZip)) &
      "#checkout-email" #> text(email, userEmail => email = userEmail.trim) &
      "#checkout-email" #> {
        if (existingUser_?) {
          "^ [class+]" #> "disabled" &
          "^ [disabled]" #> "disabled"
        } else {
          "^ [class+]" #> ""
        }
      } &
      ".billing-container" #> {
        ".form-row [class+]" #> {
          if (existingUser_? && useExistingCard)
            "hide-card"
          else
            ""
        } &
        ".existing-card" #> ClearNodesIf(!existingUser_? || !useExistingCard) &
        ".existing-card .change-card [onclick]" #> ajaxInvoke(useNewCard _)
      } &
      "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
      ".buy-box" #> ajaxSubmit("Place Order", () => orderBox)
    }
  }
}
