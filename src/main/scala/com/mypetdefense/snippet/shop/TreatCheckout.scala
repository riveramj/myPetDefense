package com.mypetdefense.snippet.shop

import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import com.mypetdefense.snippet.MyPetDefenseEvent
import com.mypetdefense.snippet.signup.Success
import com.mypetdefense.util.{ClearNodesIf, SecurityContext}
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers._

object TreatCheckout extends Loggable {
  import net.liftweb.sitemap._

  val menu = Menu.i("Treat Checkout") / "treat-checkout"
}

case object UseNewCard extends MyPetDefenseEvent("use-new-card")

class TreatCheckout extends Loggable {

  var cartRenderer: Box[IdMemoizeTransform] = Empty

  var user = SecurityContext.currentUser
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

  var discountCode = ""
  var discount_? = false

  var password = ""

  var taxDue = 0D
  var taxRate = 0D
  var priceAdditionsRenderer: Box[IdMemoizeTransform] = None
  var billingCardRenderer: Box[IdMemoizeTransform] = None
  var checkoutRenderer: Box[IdMemoizeTransform] = None
  var loginRenderer: Box[IdMemoizeTransform] = None

  var stripeToken = ""

  def findSubtotal = {
    val cart = TreatsFlow.treatShoppingCart.is

    cart.map { case (treat, quantity) =>
      treat.price.get * quantity
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

  def validateCouponCode() = {
    if (discountCode.toLowerCase == "mpd20") {
      discount_? = true

      (
        Alert("Discount applied.") &
        priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
      )
    } else
      Alert("Invalid discount code.")
  }

  def orderTreat() = {
    val validateFields = List(
        validEmailFormat(email, "#checkout-email"),
        checkEmpty(firstName, "#first-name"),
        checkEmpty(lastName, "#last-name"),
        checkEmpty(street1, "#street-1"),
        checkEmpty(city, "#city"),
        checkEmpty(state, "#state"),
        checkEmpty(zip, "#zip")
      ).flatten

    val percentOff = discountCode.toLowerCase match {
      case "mpd20" => .20
      case "mpd 20" => .20
      case _ => 0
    }

    val dollarOff = findSubtotal * percentOff

    if(!validateFields.isEmpty) {
      validateFields.foldLeft(Noop)(_ & _)
    } else {
      val stripeId = user.map(_.stripeId.get).openOr("")
      val amountPaid = findSubtotal + taxDue - dollarOff

      val treatCharge = {
        val stripeAmount = (amountPaid * 100).toLong
        val internalSaleDescription = "Treat Purchase"

        if (existingUser_? && useExistingCard) {
          val stripeCustomer = ParentService.getStripeCustomer(stripeId)

          ParentService.chargeStripeCustomer(
            stripeAmount,
            stripeCustomer.map(_.id),
            internalSaleDescription
          )
        } else if (existingUser_?) {
          val stripeCustomer = ParentService.getStripeCustomer(stripeId)

          ParentService.chargeStripeCustomerNewCard(
            stripeAmount,
            stripeCustomer.map(_.id),
            stripeToken,
            internalSaleDescription
          )
        } else {
          ParentService.chargeGuestCard(
            stripeAmount,
            stripeToken,
            internalSaleDescription
          )
        }
      }

      (for {
        charge <- treatCharge
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

        val cart = TreatsFlow.treatShoppingCart.is

        val newTreatOrder = TreatOrder.createTreatOrder(
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

        EmailActor ! TreatReceiptEmail(newTreatOrder)

        TreatsFlow.treatSale(Full(amountPaid, cart))

        S.redirectTo(Success.menu.loc.calcDefaultHref)
      }).openOr(Noop)
    }
  }

  def useNewCard() = {
    useExistingCard = false

    UseNewCard
  }

  def removeTreatFromCart(treat: Product) = {
    val cart = TreatsFlow.treatShoppingCart.is

    TreatsFlow.treatShoppingCart(cart - treat)

    (
      cartRenderer.map(_.setHtml).openOr(Noop) &
      priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
    )
  }

  def updateCartCount(treat: Product, newQuantity: Int) = {
    val cart = TreatsFlow.treatShoppingCart.is

    val updatedCart = {
      if (newQuantity < 1)
        cart - treat
      else
        cart + (treat -> newQuantity)
    }

    TreatsFlow.treatShoppingCart(updatedCart)

    (
      cartRenderer.map(_.setHtml).openOr(Noop) &
      priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
    )
  }

  def login = {
    val loginResult = LoginService.login(email, password, "", true)

    if (loginResult == Noop) {
      (
        checkoutRenderer.map(_.setHtml).openOr(Noop) &
        loginRenderer.map(_.setHtml).openOr(Noop)
      )
    } else {
      loginResult
    }
  }

  val loginBindings = {
    ".login-popover-container" #> SHtml.idMemoize { renderer =>
      loginRenderer = Full(renderer)
      val user = SecurityContext.currentUser

      ".login-popover" #> ClearNodesIf(!user.isEmpty) &
      ".login-popover #login-container" #> {
        "#email" #> SHtml.text(email, email = _) &
        "#password" #> SHtml.password(password, password = _) &
        "#login" #> SHtml.ajaxSubmit("Log In", () => login)
      }
    }
  }

  def render = {
    val orderSummary = {
      "#order-summary" #> SHtml.idMemoize { renderer =>
        priceAdditionsRenderer = Full(renderer)
        val cart = TreatsFlow.treatShoppingCart.is

        val beforeSubtotal = findSubtotal

        val subtotal = {
          if (discount_?) {
            val coupon = beforeSubtotal * .20
            beforeSubtotal - coupon
          } else {
            beforeSubtotal
          }
        }

        val total = subtotal + taxDue

        ".treat-ordered" #> cart.map { case (treat, quantity) =>
          val treatTotal = quantity * treat.price.get
          ".ordered-quantity *" #> quantity &
          ".ordered-treat-name *" #> treat.name.get &
          ".treat-total *" #> f"$$$treatTotal%2.2f"
        } &
        ".subtotal-amount *" #> f"$$$subtotal%2.2f" &
        "#tax" #> ClearNodesIf(taxDue == 0D) &
        ".tax-amount *" #> f"$$$taxDue%2.2f" &
        ".order-amount *" #> f"$$$total%2.2f"
      }
    }

    SHtml.makeFormsAjax andThen
    loginBindings &
    "#shopping-cart" #> idMemoize { renderer =>
      val cart = TreatsFlow.treatShoppingCart.is

      cartRenderer = Full(renderer)

      val subtotal = cart.map { case (treat, quantity) =>
        quantity * treat.price.get
      }.foldLeft(0D)(_ + _)

      ".items-in-cart .cart-item" #> cart.map { case (treat, quantity) =>
        val itemPrice = treat.price.get * quantity

        ".cart-treat-name *" #> treat.name.get &
        ".selected-quantity *" #> quantity &
        ".remove-treat [onclick]" #> ajaxInvoke(() => removeTreatFromCart(treat)) &
        ".subtract [onclick]" #> ajaxInvoke(() => updateCartCount(treat, quantity - 1)) &
        ".add [onclick]" #> ajaxInvoke(() => updateCartCount(treat, quantity + 1)) &
        ".treat-price *" #> f"$$$itemPrice%2.2f"
      } &
      ".cart-footer" #> {
        ".subtotal *" #> f"$$$subtotal%2.2f" &
        ".cart-actions .continue-shopping [href]" #> "/treats"
      } &
      ".items-in-cart .subtotal-container .subtotal *" #> f"$$$subtotal%2.2f" &
      ".cart-actions .continue-shopping [href]" #> "/treats" &
      ".empty-cart .continue-shopping [href]" #> "/treats" &
      ".items-in-cart" #> ClearNodesIf(cart.isEmpty) &
      ".cart-footer" #> ClearNodesIf(cart.isEmpty) &
      ".cart-actions" #> ClearNodesIf(cart.isEmpty) &
      ".empty-cart" #> ClearNodesIf(!cart.isEmpty)
    } &
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
      "#checkout-discount" #> ajaxText(discountCode, discount => discountCode = discount.trim) &
      ".apply-promo [onClick]" #> SHtml.ajaxInvoke(() => validateCouponCode()) &
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
        ".existing-card span *" #> "Card on file" &
        ".existing-card .change-card [onclick]" #> ajaxInvoke(useNewCard _)
      } &
      "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
      ".buy-treat" #> ajaxSubmit("Place Order", () => orderTreat)
    }
  }
}
