package com.mypetdefense.snippet.signup

import com.mypetdefense.model.AnimalSize.sizeToCategory
import com.mypetdefense.model.Coupon
import com.mypetdefense.service.CheckoutService.{setupNewUser, tryToCreateUser, updateSessionVars}
import com.mypetdefense.service.PetFlowChoices.cart
import com.mypetdefense.service.ValidationService.checkDuplicateIpAddress
import com.mypetdefense.service.{PetFlowChoices, StripeFacade, TaxJarService}
import com.mypetdefense.snippet.{CallableFunction, MyPetDefenseEvent}
import com.mypetdefense.util.Paths.completedPet
import net.liftweb.common._
import net.liftweb.http.SHtml.{ajaxText, hidden}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.http.{IdMemoizeTransform, S, SHtml}
import net.liftweb.json.{DefaultFormats, JValue, Serialization}
import net.liftweb.mapper.By
import net.liftweb.sitemap.Menu
import net.liftweb.util.Helpers._


object CartReview extends Loggable {
  import net.liftweb.sitemap._

  val menu = Menu.i("Cart Review") / "cart-review" >>
    completedPet
}

case class CartItem(label: String, amount: Int)
case class UpdateCartItems(items: List[CartItem], total: CartItem) extends MyPetDefenseEvent("update-cart-items")
case class StripePaymentStatus(status: String, successUrl: String) extends MyPetDefenseEvent("stripe-payment-status")
case class StripePartialAddress(city: String, region: String, postalCode: String)
case class StripeShippingAddress(recipient: String, addressLine: Array[String], city: String, region: String, postalCode: String, phone: String)
case class StripeBillingDetails(city: String, region: String, postalCode: String)

class CartReview extends Loggable {
  implicit val formats: DefaultFormats.type = DefaultFormats

  var shoppingCart = cart.is
  var shoppingCartRenderer: Box[IdMemoizeTransform] = Empty
  var partialAddress = StripePartialAddress("", "", "")
  var taxDue: BigDecimal = 0d
  var taxRate: BigDecimal = 0d
  var subtotal: BigDecimal = 0
  var todayAmount: Int = 0
  var monthlyAmount: Int = 0
  var ipAddress = ""
  var coupon: Box[Coupon] = PetFlowChoices.coupon.is
  var couponCode: String  = {
    val code = coupon.map(_.couponCode.get.toLowerCase()).getOrElse("")

    if (code == "100off")
      ""
    else
      code
  }

  def navTo(destination: Menu.Menuable) =
    SHtml.ajaxInvoke(() => S.redirectTo(destination.loc.calcDefaultHref))

  def removePet(petId: Long)() = {
    shoppingCart.remove(petId)
    cart(shoppingCart)

    shoppingCartRenderer.map(_.setHtml()).openOr(Noop)
  }

  def updateCartWithAddress(rawPartialAddress: String): JsCmd = {
    for {
      requestJson <- tryo(Serialization.read[JValue](rawPartialAddress)) ?~! "Invalid JSON." ~> 400
      addressJson <- tryo(requestJson.extract[StripePartialAddress]) ?~ "Error in address json." ~> 400
    } yield {
      partialAddress = addressJson
      calculateTax
    }

    updateCartItems
  }

  def convertStripeShippingAddess(stripeShippingAddress: StripeShippingAddress) = {
    val street1 = stripeShippingAddress.addressLine(0)
    val street2 = if (stripeShippingAddress.addressLine.length >1)
      stripeShippingAddress.addressLine(0)
    else
      ""

    NewUserAddress(
      street1,
      street2,
      stripeShippingAddress.city,
      stripeShippingAddress.region,
      stripeShippingAddress.postalCode
    )
  }

  def nameSplit(name: String) = {
    name.split(" ").toList match {
      case first :: last :: Nil =>
        (first, last)
      case first :: second :: third :: Nil =>
        (s"$first $second", third)
      case other =>
        (other.headOption.getOrElse(""), other.tail.mkString(" "))
    }
  }

  private def handleStripeFailureOnSignUp(
    stripeFailure: Box[StripeFacade.CustomerWithSubscriptions]
  ) = {
    logger.error("create customer failed with: " + stripeFailure)

    StripePaymentStatus("fail", "")
  }

  def payAndCreateAccount(arguments: String): JsCmd = {
    arguments.split("\\|", 3).toList match {
      case paymentMethodId :: rawEmail :: shippingAddress :: Nil =>
        (for {
          shippingJson <- tryo(Serialization.read[JValue](shippingAddress))
          stripeShippingAddress <- tryo(shippingJson.extract[StripeShippingAddress])
        } yield {
          val petPrices = shoppingCart.values.map(_.price).toList
          val email = "testemail8@gmail.com"

          tryToCreateUser("couponCode", petPrices, Empty, Full(paymentMethodId), Empty, email, taxRate.toDouble) match {
            case Full(customer) =>
              val address = convertStripeShippingAddess(stripeShippingAddress)
              val (firstName, lastName) = nameSplit(stripeShippingAddress.recipient)
              val userData = NewUserData(email, firstName, lastName, "", address, Empty, "")

              setupNewUser(customer, cart.values.map(_.pendingPet).toList, userData, Empty)

              val todayTotal = BigDecimal.decimal(todayAmount/100d)
              val monthlyTotal = BigDecimal.decimal(monthlyAmount/100d)

              updateSessionVars(cart.values.size, monthlyTotal, todayTotal)

              StripePaymentStatus("success", Success.menu.loc.calcDefaultHref)
            case stripeFailure => handleStripeFailureOnSignUp(stripeFailure)
          }
        }).openOr(Noop)
      case _ =>
        println("wrong number of arguments")
        println(arguments)
        println("wrong number of arguments")
        StripePaymentStatus("fail", "")
    }
  }

  def updateSubtotal() =
    subtotal = shoppingCart.values.map(_.price.price.get).sum

  def calculateTax = {
    updateSubtotal()

    if (partialAddress.postalCode.nonEmpty) {
      val taxInfo = TaxJarService.findTaxAmountAndRate(
        partialAddress,
        subtotal
      )

      taxDue = taxInfo._1
      taxRate = taxInfo._2
    }
  }

  def updateCartItems: JsCmd = {
    updateSubtotal()

    val taxAmount = tryo((taxDue * 100).toInt).openOr(0)
    val subtotalAmount = tryo((subtotal * 100).toInt).openOrThrowException("No subtotal. Please try again or contact support.")
    todayAmount = subtotalAmount + taxAmount

    val items = shoppingCart.values.map { checkoutPet =>
      val price = tryo((checkoutPet.price.price.get * 100).toInt).openOrThrowException("Error retrieving prices. Please try again.")
      CartItem(checkoutPet.pendingPet.boxType.toString, price)
    }.toList ++ List(CartItem("Tax", taxAmount))

    val total = CartItem("Due Today", todayAmount)

    UpdateCartItems(items, total)
  }

  def validateCouponCode(): JsCmd = {
    val possibleCoupon = Coupon.find(By(Coupon.couponCode, couponCode.toLowerCase()))
    val possibleCode = possibleCoupon.map(_.couponCode.get).openOr("")
    val duplicateIpAddress = if (coupon.isDefined) checkDuplicateIpAddress(ipAddress, "#ip-address-error") else Empty

    if (possibleCoupon.isEmpty || possibleCode == "100off") {
      PromoCodeMessage("error")
    } else if (duplicateIpAddress.isDefined) {
      PromoCodeMessage("error") &
      duplicateIpAddress.toList.foldLeft(Noop)(_ & _)
    } else {
      coupon = possibleCoupon
      PetFlowChoices.coupon(coupon)

      PromoCodeMessage("success") &
      shoppingCartRenderer.map(_.setHtml).openOr(Noop)
    }
  }


  def render = {
    "#ip-address" #> hidden(ipAddress = _, ipAddress) &
    "#shopping-cart" #> SHtml.idMemoize { renderer =>
      shoppingCartRenderer = Full(renderer)

      val subtotal = shoppingCart.values.map(_.price.price.get).sum

      val successCoupon = {
        if (!coupon.isEmpty)
          "promo-success"
        else
          ""
      }

      S.appendJs(updateCartItems)

      ".pet" #> shoppingCart.map { case (petId, checkoutPet) =>
        val pet = checkoutPet.pendingPet.pet
        val price = checkoutPet.price

        ".pet-name-actions" #> {
          ".name *" #> pet.name.get &
          ".remove-pet [onclick]" #> SHtml.ajaxInvoke(removePet(petId))
      } &
        ".pet-plan" #> {
          ".plan *" #> s"${checkoutPet.pendingPet.boxType.toString} Protection" &
          ".plan-size *" #> s"for ${sizeToCategory(pet.size.get)} ${pet.animalType.get}s"
        } &
        ".pet-price .price *" #> f"${price.price.get}%.2f"
      } &
      ".promo-container" #> {
        ".apply-promo-container" #> {
          ".promo-code" #> ajaxText(couponCode, couponCode = _) &
          ".apply-promo [onClick]" #> SHtml.ajaxInvoke(() => validateCouponCode())
        } &
        ".promotion-info [class+]" #> successCoupon
      } &
      ".totals" #> {
        "#subtotal .amount *" #> f"$subtotal%.2f"
      } &
      ".next-actions" #> {
        "#add-pet [onclick]" #> navTo(PetDetails.menu) &
        "#checkout [onclick]" #> navTo(Checkout.menu)
      } &
      ".update-billing-amount *" #> new CallableFunction("updateBillingAmount", updateCartWithAddress) &
      ".attach-payment-method *" #> new CallableFunction("attachPaymentMethod", payAndCreateAccount)
    }
  }
}
