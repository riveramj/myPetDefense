package com.mypetdefense.snippet.signup

import java.util.Date

import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.service.PetFlowChoices._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import com.mypetdefense.snippet.MyPetDefenseEvent
import com.mypetdefense.util.{ClearNodesIf, SecurityContext}
import me.frmr.stripe.{Customer, StripeExecutor}
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._
import net.liftweb.util._

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure => TryFail, Success => TrySuccess, _}
import scala.xml.NodeSeq

object Checkout extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Checkout") / "checkout" >>
    completedPet
}

case class PromoCodeMessage(status: String) extends MyPetDefenseEvent("promotion-code-message")

class Checkout extends Loggable {
  val stripeSecretKey: String    = Props.get("secret.key") openOr ""
  implicit val e: StripeExecutor = new StripeExecutor(stripeSecretKey)

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
  var couponCode: String = {
    if (is50Off(coupon))
      ""
    else
      coupon.map(_.couponCode.get.toLowerCase()).openOr("")
  }

  val pets: mutable.LinkedHashMap[Long, Pet] = completedPets.is
  val petCount: Int                          = pets.size

  val smMedPets: Int = pets.values.count { pet =>
    pet.size.get != AnimalSize.DogLargeZo && pet.size.get != AnimalSize.DogXLargeZo
  }

  val lgXlPets: Int = petCount - smMedPets

  val subtotal: Double = (smMedPets * 24.99) + (lgXlPets * 27.99)
  val discount: Double = petCount match {
    case 0 | 1 => 0
    case _     => subtotal * 0.1
  }
  val promotionAmount: Double      = coupon.map(_.percentOff.get).openOr(0).toDouble / 100 * subtotal
  val subtotalWithDiscount: Double = subtotal - discount

  val pennyCount: Int = (subtotal * 100).toInt

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

    val taxInfo = TaxJarService.findTaxAmoutAndRate(
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
    val passwordError = checkEmpty(password, "#password")
    val facebookError = checkFacebookId(facebookId, "#facebook-id", true)

    val baseFields = List(
      checkEmpty(firstName, "#first-name"),
      checkEmpty(lastName, "#last-name"),
      checkEmpty(email, "#email"),
      checkEmpty(street1, "#street-1"),
      checkEmpty(city, "#city"),
      checkEmpty(state, "#state"),
      checkEmpty(zip, "#zip")
    )

    val validateFields = {
      if (facebookId.isEmpty)
        passwordError :: baseFields
      else
        facebookError :: baseFields
    }.flatten

    if (validateFields.isEmpty) {
      (coupon, petCount) match {
        case (Full(_), _) =>
        case (Empty, 2) =>
          coupon = Coupon.find(By(Coupon.couponCode, "multiPet"))
        case (_, _) =>
      }

      val couponId = coupon.map(_.couponCode.get)

      val stripeCustomer = {
        if (couponId.isEmpty) {
          Customer.create(
            email = Some(email),
            card = Some(stripeToken),
            plan = Some("pennyProduct"),
            quantity = Some(pennyCount),
            taxPercent = Some(taxRate)
          )
        } else {
          Customer.create(
            email = Some(email),
            card = Some(stripeToken),
            plan = Some("pennyProduct"),
            quantity = Some(pennyCount),
            taxPercent = Some(taxRate),
            coupon = couponId
          )
        }
      }

      Try(Await.result(stripeCustomer, new DurationInt(7).seconds)) match {
        case TrySuccess(Full(customer)) =>
          newUserSetup(customer)

          PetFlowChoices.petCount(Full(petCount))

          PetFlowChoices.completedPets(mutable.LinkedHashMap.empty)

          val total = subtotalWithDiscount + taxDue

          PetFlowChoices.total(Full(total))

          PetFlowChoices.freeMonths(coupon.map(_.numberOfMonths.get))

          S.redirectTo(Success.menu.loc.calcDefaultHref)

        case TrySuccess(stripeFailure) =>
          logger.error("create customer failed with: " + stripeFailure)
          Alert(s"""An error has occurred $stripeFailure. Please Try again.""")

        case TryFail(throwable: Throwable) =>
          logger.error("create customer failed with: " + throwable)
          Alert("An error has occured. Please try again.")
      }
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def createNewPets(user: Box[User]): List[Pet] = {
    for {
      usr <- user.toList
      pet <- pets.values
    } yield {
      Pet.createNewPet(pet, usr)
    }
  }

  def newUserSetup(customer: Customer): Box[User] = {
    val stripeId = customer.id

    val user = if (currentUser.isEmpty) {
      Full(
        User.createNewUser(
          firstName,
          lastName,
          stripeId,
          email,
          password,
          "",
          coupon,
          coupon.flatMap(_.agency.obj),
          None,
          UserType.Parent
        )
      )
    } else {
      currentUser.flatMap(_.refresh).map { oldUser =>
        oldUser
          .firstName(firstName)
          .lastName(lastName)
          .stripeId(stripeId)
          .email(email)
          .coupon(coupon)
          .referer(coupon.flatMap(_.agency.obj))
          .saveMe
      }
    }

    Address.createNewAddress(
      user,
      street1,
      street2,
      city,
      state,
      zip,
      AddressType.Shipping
    )

    val pets = createNewPets(user)

    val subscriptionId = (for {
      rawSubscriptions <- customer.subscriptions
      subscription     <- rawSubscriptions.data.headOption
    } yield {
      subscription.id
    }).flatten.getOrElse("")

    val mpdSubscription = Subscription.createNewSubscription(
      user,
      subscriptionId,
      new Date(),
      new Date(),
      priceCode.is.openOr(Price.defaultPriceCode),
      isUpgraded = true
    )

    val userWithSubscription = user.map(_.subscription(mpdSubscription).saveMe())

    val boxes = pets.map { pet =>
      val box = SubscriptionBox.createNewBox(mpdSubscription, pet)
      pet.box(box).saveMe()

      box
    }

    boxes.map(SubscriptionItem.createFirstBox)

    if (Props.mode == Props.RunModes.Production) {
      EmailActor ! NewSaleEmail(
        userWithSubscription,
        petCount,
        coupon.map(_.couponCode.get).openOr("")
      )
    }

    EmailActor ! SendWelcomeEmail(userWithSubscription)

    userWithSubscription.flatMap(_.refresh).map(SecurityContext.logIn)

    userWithSubscription
  }

  def render: NodeSeq => NodeSeq = {
    val orderSummary = {
      "#order-summary" #> SHtml.idMemoize { renderer =>
        priceAdditionsRenderer = Full(renderer)

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
      if (!coupon.isEmpty && !coupon.equals(Coupon.halfOffCoupon))
        "promo-success"
      else
        ""
    }

    SHtml.makeFormsAjax andThen
      orderSummary &
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
