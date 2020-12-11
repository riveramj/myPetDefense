package com.mypetdefense.snippet.signup

import com.mypetdefense.model._
import com.mypetdefense.service.PetFlowChoices._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import com.mypetdefense.snippet.MyPetDefenseEvent
import com.mypetdefense.util.{ClearNodesIf, SecurityContext}
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._
import net.liftweb.util._

import scala.collection.mutable
import scala.xml.NodeSeq

object Checkout extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Checkout") / "checkout" >>
    completedPet
}

case class PromoCodeMessage(status: String) extends MyPetDefenseEvent("promotion-code-message")

class Checkout extends Loggable {
  var currentUser: Box[User] = SecurityContext.currentUser

  var email: String                                   = currentUser.map(_.email.get).openOr("")
  var firstName: String                               = currentUser.map(_.firstName.get).getOrElse("")
  var lastName: String                                = currentUser.map(_.lastName.get).getOrElse("")
  var password                                        = ""
  var facebookId                                      = ""
  var street1                                         = ""
  var street2                                         = ""
  var city                                            = ""
  var state                                           = ""
  var zip                                             = ""
  var taxRate                                         = 0d
  var taxDue                                          = 0d
  var priceAdditionsRenderer: Box[IdMemoizeTransform] = None
  var accountRenderer: Box[IdMemoizeTransform]        = None

  var stripeToken         = ""
  var coupon: Box[Coupon] = PetFlowChoices.coupon.is
  var couponCode: String  = coupon.map(_.couponCode.get.toLowerCase()).openOr("")

  val pets: mutable.LinkedHashMap[Long, Pet] = completedPets.is
  val petCount: Int                          = pets.size

  val smMedPets: Int = pets.values.count { pet =>
    pet.size.get != AnimalSize.DogLargeZo && pet.size.get != AnimalSize.DogXLargeZo
  }

  val lgXlPets: Int = petCount - smMedPets

  val subtotal: BigDecimal = (smMedPets * BigDecimal(24.99)) + (lgXlPets * BigDecimal(27.99))
  val discount: BigDecimal = petCount match {
    case 0 | 1 => BigDecimal(0)
    case _     => subtotal * 0.1
  }
  var promotionAmount: BigDecimal      = (coupon.map(_.percentOff.get).openOr(0) / 100d) * subtotal
  val subtotalWithDiscount: BigDecimal = subtotal - discount

  val pennyCount: Int = (subtotal * 100).toInt

  private def handleStripeFailureOnSignUp(
      stripeFailure: Box[StripeFacade.CustomerWithSubscriptions]
  ): Alert = {
    logger.error("create customer failed with: " + stripeFailure)
    Alert(s"""An error has occurred $stripeFailure. Please Try again.""")
  }

  private def updateSessionVars() = {
    val total = subtotalWithDiscount + taxDue
    PetFlowChoices.petCount(Full(petCount))
    PetFlowChoices.completedPets(mutable.LinkedHashMap.empty)
    PetFlowChoices.total(Full(total))
    PetFlowChoices.freeMonths(coupon.map(_.numberOfMonths.get))
  }

  private def setupNewUserAndRedirect(customer: StripeFacade.CustomerWithSubscriptions): Nothing = {
    val newUserAddress          = NewUserAddress(street1, street2, city, state, zip)
    val newUserData             = NewUserData(email, firstName, lastName, password, newUserAddress, coupon)
    val petsToCreate            = pets.values.toList
    val priceCodeOfSubscription = priceCode.is.openOr(Price.defaultPriceCode)
    val userWithSubscription = CheckoutService.newUserSetup(
      currentUser,
      petsToCreate,
      priceCodeOfSubscription,
      newUserData,
      customer
    )
    userWithSubscription.map(_.reload).map(SecurityContext.logIn)
    updateSessionVars()
    S.redirectTo(Success.menu.loc.calcDefaultHref)
  }

  private def setMultiPetCouponIfPossible(): Unit = (coupon, petCount) match {
    case (Empty, 2) => coupon = Coupon.find(By(Coupon.couponCode, "multiPet"))
    case (_, _)     => ()
  }

  private def tryToCreateUser = {
    setMultiPetCouponIfPossible()

    val stripeCustomer =
      StripeFacade.Customer.createWithSubscription(
        email,
        stripeToken,
        priceId = "pennyProduct",
        pennyCount,
        taxRate,
        coupon
      )

    stripeCustomer match {
      case Full(customer) => setupNewUserAndRedirect(customer)
      case stripeFailure  => handleStripeFailureOnSignUp(stripeFailure)
    }
  }

  private def validateFields: List[ValidationError] = {
    val passwordError = checkEmpty(password, "#password")
    val facebookError = checkFacebookId(facebookId, "#facebook-id", signup = true)

    val baseFields = List(
      checkEmpty(firstName, "#first-name"),
      checkEmpty(lastName, "#last-name"),
      checkEmail(email, "#email", signup = true),
      checkEmpty(street1, "#street-1"),
      checkEmpty(city, "#city"),
      checkEmpty(state, "#state"),
      checkEmpty(zip, "#zip")
    )

    val validationResult = {
      if (facebookId.isEmpty)
        passwordError :: baseFields
      else
        facebookError :: baseFields
    }.flatten

    validationResult
  }

  def codeMatchesCoupon(code: String, coupon: Box[Coupon]): Boolean =
    coupon.map(_.couponCode.get).contains(code)

  def is50Off(coupon: Box[Coupon]): Boolean =
    codeMatchesCoupon(Coupon.halfOffCouponCode, coupon)

  def validateCouponCode(): JsCmd = {
    val possibleCoupon = Coupon.find(By(Coupon.couponCode, couponCode.toLowerCase()))

    if (possibleCoupon.isEmpty) {
      PromoCodeMessage("error")
    } else {
      coupon = possibleCoupon
      PetFlowChoices.coupon(coupon)

      PromoCodeMessage("success") &
        priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
    }
  }

  def connectFacebook(): JsCmd = {
    val newUser = User.upsertUser(
      firstName,
      lastName,
      email,
      "",
      facebookId,
      UserType.Parent
    )

    SecurityContext.logIn(newUser)
    currentUser = Full(newUser)

    accountRenderer.map(_.setHtml()).openOr(Noop)
  }

  def calculateTax(possibleState: String, possibleZip: String): JsCmd = {
    state = possibleState
    zip = possibleZip

    val taxInfo = TaxJarService.findTaxAmountAndRate(
      city,
      state,
      zip,
      subtotalWithDiscount
    )

    taxDue = taxInfo._1
    taxRate = taxInfo._2

    priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
  }

  def signup(): JsCmd = {
    val validationErrors = validateFields
    if (validationErrors.isEmpty) tryToCreateUser
    else validationErrors.foldLeft(Noop)(_ & _)
  }

  def render: NodeSeq => NodeSeq = {
    val orderSummary = {
      "#order-summary" #> SHtml.idMemoize { renderer =>
        priceAdditionsRenderer = Full(renderer)

        promotionAmount = (coupon.map(_.percentOff.get).openOr(0) / 100d) * subtotal

        val monthlyTotal = subtotalWithDiscount + taxDue
        val todayTotal   = subtotalWithDiscount + taxDue - promotionAmount

        "#subtotal span *" #> f"$$$subtotal%2.2f" &
          "#discount" #> ClearNodesIf(discount == 0) &
          "#promotion" #> ClearNodesIf(promotionAmount == 0) &
          "#promotion span *" #> f"-$$$promotionAmount%2.2f" &
          "#discount span *" #> f"$$$discount%2.2f" &
          "#tax" #> ClearNodesIf(taxDue == 0d) &
          "#tax span *" #> f"$$$taxDue%2.2f" &
          "#monthly-total span *" #> f"$$$monthlyTotal%2.2f" & {
          val couponDiscountPercent = coupon.map(_.percentOff.get).openOr(0)
          val couponMonthCount      = coupon.map(_.numberOfMonths.get).openOr(0)

          if (coupon.isEmpty || couponDiscountPercent != 100) {
            "#order span *" #> f"$$$todayTotal%2.2f"
          } else {
            "#order span *" #> {
              if (couponMonthCount == 1) {
                s"First Month Free"
              } else {
                s"""First $couponMonthCount months free"""
              }
            }
          }
        }
      }
    }

    val successCoupon = {
      if (!coupon.isEmpty)
        "promo-success"
      else
        ""
    }

    SHtml.makeFormsAjax andThen
      orderSummary andThen
      "#left-column" #> SHtml.idMemoize { renderer =>
        accountRenderer = Full(renderer)

        {
          if (currentUser.isEmpty)
            "#password" #> SHtml.password(password, userPassword => password = userPassword.trim)
          else {
            ".password-container" #> ClearNodes &
              ".facebook-option" #> ClearNodes &
              "#facebook-id" #> ClearNodes &
              "#email [disabled]" #> "disabled"
          }
        } andThen
          "#email" #> ajaxText(email, userEmail => email = userEmail.trim) &
            "#facebook-id" #> ajaxText(facebookId, facebookId = _) &
            "#first-name" #> ajaxText(firstName, firstName = _) &
            "#last-name" #> ajaxText(lastName, lastName = _) &
            ".connect-facebook [onClick]" #> SHtml.ajaxInvoke(() => connectFacebook()) &
            "#street-1" #> text(street1, street1 = _) &
            "#street-2" #> text(street2, street2 = _) &
            "#city" #> ajaxText(city, city = _) &
            "#state" #> ajaxText(state, possibleState => calculateTax(possibleState, zip)) &
            "#zip" #> ajaxText(zip, possibleZip => calculateTax(state, possibleZip))
      } andThen
      "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
        ".checkout" #> SHtml.ajaxSubmit("Place Order", () => signup()) &
        ".promotion-info [class+]" #> successCoupon &
        "#promo-code" #> ajaxText(couponCode, couponCode = _) &
        ".apply-promo [onClick]" #> SHtml.ajaxInvoke(() => validateCouponCode())
  }
}
