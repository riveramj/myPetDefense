package com.mypetdefense.snippet.signup

import com.mypetdefense.model._
import com.mypetdefense.model.domain.action.CustomerAction.{CustomerAddedPet, CustomerSignedUp}
import com.mypetdefense.service.PetFlowChoices._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import com.mypetdefense.snippet.MyPetDefenseEvent
import com.mypetdefense.util.{ClearNodesIf, DateHelper, SecurityContext}
import net.liftweb.common.Box.box2Iterable
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
  var ipAddress                                       = ""
  var taxRate                                         = 0d
  var taxDue                                          = 0d
  var monthlyTotal: BigDecimal                        = 0
  var todayTotal: BigDecimal                          = 0
  var priceAdditionsRenderer: Box[IdMemoizeTransform] = None
  var accountRenderer: Box[IdMemoizeTransform]        = None

  var stripeToken         = ""
  var coupon: Box[Coupon] = PetFlowChoices.coupon.is
  var couponCode: String  = {
    val code = coupon.map(_.couponCode.get.toLowerCase()).getOrElse("")

    if (code == "100off")
      ""
    else
      code
  }

  val pets: mutable.LinkedHashMap[Long, Pet] = completedPets.is
  val petCount: Int                          = pets.size

  val smallDogCount: Int = pets.values.count {_.size.get == AnimalSize.DogSmallZo}
  val nonSmallDogCount: Int = petCount - smallDogCount

  val subtotal: BigDecimal = (smallDogCount * BigDecimal(24.99)) + (nonSmallDogCount * BigDecimal(27.99))

  var promotionAmount: BigDecimal = findPromotionAmount()

  val pennyCount: Int = (subtotal * 100).toInt

  private def handleStripeFailureOnSignUp(
      stripeFailure: Box[StripeFacade.CustomerWithSubscriptions]
  ): Alert = {
    logger.error("create customer failed with: " + stripeFailure)
    Alert(s"""An error has occurred $stripeFailure. Please Try again.""")
  }

  private def updateSessionVars() = {

    PetFlowChoices.petCount(Full(petCount))
    PetFlowChoices.completedPets(mutable.LinkedHashMap.empty)
    PetFlowChoices.monthlyTotal(Full(monthlyTotal))
    PetFlowChoices.todayTotal(Full(todayTotal))
  }

  private def findPromotionAmount(): BigDecimal = {
    (coupon.map(_.percentOff.get), coupon.map(_.dollarOff.get)) match {
      case (Full(percent), _) if percent > 0 =>
        (coupon.map(_.percentOff.get).openOr(0) / 100d) * subtotal

      case (_, Full(dollarAmount)) if dollarAmount > 0 && dollarAmount < subtotal =>
        if (couponCode == "20off") {

          val smallDogPromo = smallDogCount * BigDecimal(19.99)
          val nonSmallDogPromo = nonSmallDogCount * BigDecimal(22.99)

          val totalPromoAmount = {
            if (nonSmallDogCount >= 3)
              3 * BigDecimal(22.99)
            else if ((smallDogCount + nonSmallDogCount) >= 3)
              nonSmallDogPromo + ((3 - nonSmallDogCount) * BigDecimal(19.99))
            else
              smallDogPromo + nonSmallDogPromo
          }

          totalPromoAmount
        } else dollarAmount

      case (_, Full(dollarAmount)) if dollarAmount > 0 && dollarAmount > subtotal =>
        subtotal

      case (_,_) => 0
    }
  }

  private def setupNewUserAndRedirect(customer: StripeFacade.CustomerWithSubscriptions): Nothing = {
    val newUserAddress          = NewUserAddress(street1, street2, city, state, zip)
    val newUserData             = NewUserData(email, firstName, lastName, password, newUserAddress, coupon, ipAddress)
    val petsToCreate            = pets.values.toList
    val priceCodeOfSubscription = priceCode.is.openOr(Price.defaultPriceCode)
    val userWithSubscription = CheckoutService.newUserSetup(
      currentUser,
      petsToCreate,
      priceCodeOfSubscription,
      newUserData,
      customer
    )
    val updatedUserWithSubscription = userWithSubscription.map(_.reload)
    updatedUserWithSubscription.map(SecurityContext.logIn)

    for {
      offerCode <- PetFlowChoices.woofTraxOfferCode.is
      userId <- PetFlowChoices.woofTraxUserId.is
      user <- updatedUserWithSubscription
    } yield {
      WoofTraxOrder.createWoofTraxOrder(offerCode, userId, user)
    }

    val todayDateTime = DateHelper.now.toString
    val signUpActionLog = CustomerSignedUp(
      SecurityContext.currentUserId,
      None,
      todayDateTime,
      coupon.map(_.couponCode.get)
    )
    val petActionLog = updatedUserWithSubscription.toList
      .flatMap(_.pets.toList)
      .map { pet =>
        CustomerAddedPet(
          SecurityContext.currentUserId,
          None,
          pet.petId.get,
          pet.name.get
        )
      }

    ActionLogService.logAction(signUpActionLog)
    petActionLog.foreach(ActionLogService.logAction)

    updateSessionVars()
    S.redirectTo(Success.menu.loc.calcDefaultHref)
  }

  private def tryToCreateUser = {
    val promoPennyCount = {
      if (couponCode == "20off") {
        if (nonSmallDogCount >= 3)
          (smallDogCount * 2499) + ((nonSmallDogCount - 3) * 2799) + 1500
        else if ((smallDogCount + nonSmallDogCount) >= 3)
          ((smallDogCount - (3 - nonSmallDogCount)) * 2499) + 1500
        else (smallDogCount + nonSmallDogCount) * 500
      } else
        pennyCount
    }

    val promoPennyCoupon = {
      if (couponCode == "20off")
        Empty
      else
        coupon
    }

    val stripeCustomer =
      StripeFacade.Customer.createWithSubscription(
        email,
        stripeToken,
        priceId = "pennyProduct",
        promoPennyCount,
        taxRate,
        promoPennyCoupon
      )

    stripeCustomer match {
      case Full(customer) => setupNewUserAndRedirect(customer)
      case stripeFailure  => handleStripeFailureOnSignUp(stripeFailure)
    }
  }

  private def validateFields: (List[MyPetDefenseEvent], Boolean) = {
    val passwordError = checkEmpty(password, "#password")
    val facebookError = checkFacebookId(facebookId, "#facebook-id", signup = true)
    val duplicateIpAddress = if (coupon.nonEmpty) checkDuplicateIpAddress(ipAddress, "#ip-address-error") else Empty

    val baseFields = List(
      checkEmpty(firstName, "#first-name"),
      checkEmpty(lastName, "#last-name"),
      checkEmail(email, "#email", signup = true),
      checkEmpty(street1, "#street-1"),
      checkEmpty(city, "#city"),
      checkEmpty(state, "#state"),
      checkEmpty(zip, "#zip"),
      duplicateIpAddress
    )

    val validationResult = {
      if (facebookId.isEmpty)
        passwordError :: baseFields
      else
        facebookError :: baseFields
    }.flatten

    if (duplicateIpAddress.nonEmpty)
      coupon = Empty

    (validationResult, duplicateIpAddress.nonEmpty)
  }

  def codeMatchesCoupon(code: String, coupon: Box[Coupon]): Boolean =
    coupon.map(_.couponCode.get).contains(code)

  def is50Off(coupon: Box[Coupon]): Boolean =
    codeMatchesCoupon(Coupon.halfOffCouponCode, coupon)

  def validateCouponCode(): JsCmd = {
    val possibleCoupon = Coupon.find(By(Coupon.couponCode, couponCode.toLowerCase()))
    val possibleCode = possibleCoupon.map(_.couponCode.get).openOr("")

    if (possibleCoupon.isEmpty || possibleCode == "100off") {
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
      subtotal
    )

    taxDue = taxInfo._1
    taxRate = taxInfo._2

    priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
  }

  def signup(): JsCmd = {
    val (validationErrors, duplicateIpAddress) = validateFields
    if (validationErrors.isEmpty) tryToCreateUser
    else if (duplicateIpAddress)
      priceAdditionsRenderer.map(_.setHtml).openOr(Noop) &
      PromoCodeMessage("error") &
      validationErrors.foldLeft(Noop)(_ & _)

    else
      validationErrors.foldLeft(Noop)(_ & _)
  }

  def render: NodeSeq => NodeSeq = {
    val orderSummary = {
      "#order-summary" #> SHtml.idMemoize { renderer =>
        priceAdditionsRenderer = Full(renderer)

        promotionAmount = findPromotionAmount()

        monthlyTotal = subtotal + taxDue
        todayTotal   = if (subtotal > promotionAmount)
          subtotal + taxDue - promotionAmount
        else
          subtotal - promotionAmount

        "#subtotal span *" #> f"$$$subtotal%2.2f" &
        "#promotion" #> ClearNodesIf(promotionAmount == 0) &
        "#promotion span *" #> f"-$$$promotionAmount%2.2f" &
        "#tax" #> ClearNodesIf(taxDue == 0d) &
        "#tax span *" #> f"$$$taxDue%2.2f" &
        "#monthly-total span *" #> f"$$$monthlyTotal%2.2f" & {
          if (coupon.isEmpty || subtotal - promotionAmount > 0) {
            "#order span *" #> f"$$$todayTotal%2.2f"
          } else {
            "#order span *" #> "First Month Free"
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
      "#ip-address" #> hidden(ipAddress = _, ipAddress) &
      ".checkout" #> SHtml.ajaxSubmit("Place Order", () => signup()) &
      ".promotion-info [class+]" #> successCoupon &
      "#promo-code" #> ajaxText(couponCode, couponCode = _) &
      ".apply-promo [onClick]" #> SHtml.ajaxInvoke(() => validateCouponCode())
  }
}
