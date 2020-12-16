package com.mypetdefense.snippet

import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.service._
import com.mypetdefense.snippet.signup.Success
import com.mypetdefense.util.{ClearNodesIf, SecurityContext}
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

object AddOnCheckout extends Loggable {
  import net.liftweb.sitemap._

  val menu: Menu.Menuable with Menu.WithSlash = Menu.i("Add-on Checkout") / "add-on-checkout"
}

class AddOnCheckout extends Loggable {
  var cartRenderer: Box[IdMemoizeTransform] = Empty

  val user: Box[User]                 = SecurityContext.currentUser
  val address: Box[Address]           = user.flatMap(_.shippingAddress)
  val subscription: Box[Subscription] = user.flatMap(_.subscription)
  val boxes: List[SubscriptionBox]    = subscription.map(_.subscriptionBoxes.toList).openOr(Nil)

  var discountCode = ""
  var discount_?   = false

  var taxDue  = BigDecimal(0)
  var taxRate = BigDecimal(0)

  var priceAdditionsRenderer: Box[IdMemoizeTransform] = None
  var checkoutRenderer: Box[IdMemoizeTransform]       = None

  def findNewMonthlySubtotal: Double = {
    val cart = AddOnFlow.addOnShoppingCart.is

    (cart.map {
      case (product, quantity) =>
        product.price.get * quantity
    }.foldLeft(0d)(_ + _)) + subscription.map(_.getMonthlyCost).openOr(0d)
  }

  def calculateTax(): JsCmd = {
    val possibleTaxRate = user.map(_.taxRate.get)

    val taxInfo = if (possibleTaxRate.isDefined) {
      val taxPercent = possibleTaxRate.openOr(0d)
      (findNewMonthlySubtotal * taxPercent, taxPercent)
    } else {
      TaxJarService.findTaxAmountAndRate(
        address.map(_.city.get).openOr(""),
        address.map(_.state.get).openOr(""),
        address.map(_.zip.get).openOr(""),
        findNewMonthlySubtotal
      )
    }

    taxDue = taxInfo._1
    taxRate = taxInfo._2

    priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
  }

  def validateCouponCode(): JsCmd = {
    if (discountCode.toLowerCase == "mpd20") {
      discount_? = true

      (
        Alert("Discount applied.") &
          priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
      )
    } else
      Alert("Invalid discount code.")
  }

  def orderTreat(): Unit = {
    val treatCharge = {
      val cart = AddOnFlow.addOnShoppingCart.is

      val addOnItems = cart.flatMap {
        case (product, quantity) =>
          boxes.headOption.map { box =>
            AddOnProduct.createAddOnProduct(
              product,
              box,
              quantity,
              1d
            )
          }
      }.toList

      EmailActor ! AddOnReceiptEmail(addOnItems, subscription, user)

      AddOnFlow.addOnSale(cart)

      S.redirectTo(Success.menu.loc.calcDefaultHref)
    }
  }

  def removeTreatFromCart(treat: Product): JsCmd = {
    val cart = AddOnFlow.addOnShoppingCart.is

    AddOnFlow.addOnShoppingCart(cart - treat)

    (
      cartRenderer.map(_.setHtml).openOr(Noop) &
        priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
    )
  }

  def updateCartCount(treat: Product, newQuantity: Int): JsCmd = {
    val cart = AddOnFlow.addOnShoppingCart.is

    val updatedCart = {
      if (newQuantity < 1)
        cart - treat
      else
        cart + (treat -> newQuantity)
    }

    AddOnFlow.addOnShoppingCart(updatedCart)

    (
      cartRenderer.map(_.setHtml).openOr(Noop) &
        priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
    )
  }

  def render: NodeSeq => NodeSeq = {
    val orderSummary = {
      "#order-summary" #> SHtml.idMemoize { renderer =>
        priceAdditionsRenderer = Full(renderer)
        val cart = AddOnFlow.addOnShoppingCart.is

        val beforeSubtotal = findNewMonthlySubtotal

        val subtotal = {
          if (discount_?) {
            val coupon = beforeSubtotal * .20
            beforeSubtotal - coupon
          } else {
            beforeSubtotal
          }
        }

        val total = subtotal + taxDue

        ".treat-ordered" #> cart.map {
          case (treat, quantity) =>
            val treatTotal = quantity * treat.price.get
            ".ordered-quantity *" #> quantity &
              ".ordered-treat-name *" #> treat.name.get &
              ".treat-total *" #> f"$$$treatTotal%2.2f"
        } &
          ".subtotal-amount *" #> f"$$$subtotal%2.2f" &
          "#tax" #> ClearNodesIf(taxDue == 0d) &
          ".tax-amount *" #> f"$$$taxDue%2.2f" &
          ".order-amount *" #> f"$$$total%2.2f"
      }
    }

    SHtml.makeFormsAjax andThen
      "#shopping-cart" #> idMemoize { renderer =>
        val cart = AddOnFlow.addOnShoppingCart.is

        cartRenderer = Full(renderer)

        val subtotal = cart.map {
          case (treat, quantity) =>
            quantity * treat.price.get
        }.foldLeft(0d)(_ + _)

        ".items-in-cart .cart-item" #> cart.map {
          case (treat, quantity) =>
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
              ".cart-actions .continue-shopping [href]" #> "/add-on"
          } &
          ".items-in-cart .subtotal-container .subtotal *" #> f"$$$subtotal%2.2f" &
          ".cart-actions .continue-shopping [href]" #> "/add-on" &
          ".empty-cart .continue-shopping [href]" #> "/add-on" &
          ".items-in-cart" #> ClearNodesIf(cart.isEmpty) &
          ".cart-footer" #> ClearNodesIf(cart.isEmpty) &
          ".cart-actions" #> ClearNodesIf(cart.isEmpty) &
          ".empty-cart" #> ClearNodesIf(!cart.isEmpty)
      } &
        ".checkout-container" #> SHtml.idMemoize { renderer =>
          checkoutRenderer = Full(renderer)
          orderSummary &
            "#name *" #> user.map(_.name) &
            "#street-1 *" #> address.map(_.street1.get) &
            "#street-2 *" #> address.map(_.street2.get) &
            "#street-2" #> ClearNodesIf(address.map(_.street2.get).isEmpty) &
            "#city *" #> address.map(_.city.get) &
            "#state *" #> address.map(_.state.get) &
            "#zip *" #> address.map(_.zip.get) &
            "#checkout-email" #> user.map(_.email.get) &
            "#checkout-discount" #> ajaxText(discountCode, discount => discountCode = discount.trim) &
            ".apply-promo [onClick]" #> SHtml.ajaxInvoke(() => validateCouponCode()) &
            ".buy-treat" #> ajaxSubmit("Place Order", () => orderTreat)
        }
  }
}
