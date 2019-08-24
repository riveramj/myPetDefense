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

import com.mypetdefense.service._
  import ValidationService._
  import PetFlowChoices._

import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.model._
import com.mypetdefense.actor._

import java.util.Date
import java.time.MonthDay

import scala.util.{Failure => TryFail, Success => TrySuccess, _}

import scala.concurrent.Await
import scala.concurrent.duration._

import me.frmr.stripe.{StripeExecutor, Customer, Coupon => StripeCoupon}

import dispatch._, Defaults._

object Checkout extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Checkout") / "checkout" >>
    completedPetOrFlow
}

case class PromoCodeMessage(status: String) extends MyPetDefenseEvent("promotion-code-message")

class Checkout extends Loggable {
  val stripeSecretKey = Props.get("secret.key") openOr ""
  implicit val e = new StripeExecutor(stripeSecretKey)

  var email = ""
  var password = ""
  
  var firstName = ""
  var lastName = ""
  var street1 = ""
  var street2 = ""
  var city = ""
  var state = ""
  var zip = ""
  var taxRate = 0D
  var taxDue = 0D
  var priceAdditionsRenderer: Box[IdMemoizeTransform] = None

  var stripeToken = ""
  var coupon: Box[Coupon] = PetFlowChoices.coupon
  var couponCode = coupon.map(_.couponCode.get).openOr("")

  val pets = completedPets.is
  val petCount = pets.size
  
  val subtotal = PetFlowChoices.subtotal.is.openOr(0D)
  val discount = PetFlowChoices.discount.is.openOr(0D)
  val subtotalWithDiscount = subtotal - discount

  val pennyCount = (subtotal * 100).toInt

  def calculateTax(possibleState: String, possibleZip: String) = {
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

  def signup() = {
    val validateFields = List(
        checkEmail(email, "#email"),
        checkEmpty(firstName, "#first-name"),
        checkEmpty(lastName, "#last-name"),
        checkEmpty(password, "#password"),
        checkEmpty(street1, "#street-1"),
        checkEmpty(city, "#city"),
        checkEmpty(state, "#state"),
        checkEmpty(zip, "#zip")
      ).flatten

    if(validateFields.isEmpty) {
      (coupon, petCount) match {
        case (Full(_), _) =>
        case (Empty, 1) => 
        case (Empty, 2) => 
          coupon = Coupon.find(By(Coupon.couponCode, "twopets"))
        case (Empty, manyPets) if manyPets > 2 => 
          coupon = Coupon.find(By(Coupon.couponCode, "threepets"))
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
          val user = newUserSetup(
            customer
          )

          val total = subtotalWithDiscount + taxDue
          
          PetFlowChoices.total(Full(total))
          
          PetFlowChoices.freeMonths(coupon.map(_.freeMonths.get))

          S.redirectTo(Success.menu.loc.calcDefaultHref)

        case TrySuccess(stripeFailure) =>
          logger.error("create customer failed with: " + stripeFailure)
          Alert(s"An error has occurred ${stripeFailure}. Please Try again.")

        case TryFail(throwable: Throwable) =>
          logger.error("create customer failed with: " + throwable)
          Alert("An error has occured. Please try again.")
      }
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def createNewPets(user: User) = {
    pets.map { case (petId, pet) =>
      Pet.createNewPet(pet, user)
    }
  }

  def newUserSetup(customer: Customer) = {
    val stripeId = customer.id

    val user = User.createNewUser(
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

    Address.createNewAddress(
      Full(user),
      street1,
      street2,
      city,
      state,
      zip,
      AddressType.Shipping
    )
    
    createNewPets(user)

    val subscriptionId = (
      for {
        rawSubscriptions <- customer.subscriptions
        subscription <- rawSubscriptions.data.headOption
      } yield {
        subscription.id
      }).flatMap(identity).getOrElse("")

    val mpdSubscription = Subscription.createNewSubscription(
      user,
      subscriptionId,
      new Date(),
      new Date(),
      priceCode.is.openOr(Price.defaultPriceCode)
    )

    TaggedItem.createNewTaggedItem(
      subscription = Full(mpdSubscription),
      tag = Tag.useBox
    )

    if (Props.mode == Props.RunModes.Production) {
      EmailActor ! NewSaleEmail(user, petCount, coupon.map(_.couponCode.get).openOr(""))
    }

    EmailActor ! SendWelcomeEmail(user)
    
    user
  }

  def render = {
    val orderSummary = {
      "#order-summary" #> SHtml.idMemoize { renderer =>
        priceAdditionsRenderer = Full(renderer)

        val monthlyTotal = subtotalWithDiscount + taxDue

        "#subtotal span *" #> f"$$$subtotal%2.2f" &
        "#discount" #> ClearNodesIf(discount == 0) &
        "#discount span *" #> f"$$$discount%2.2f" &
        "#tax" #> ClearNodesIf(taxDue == 0D) &
        "#tax span *" #> f"$$$taxDue%2.2f" &
        "#monthly-total span *" #> f"$$$monthlyTotal%2.2f" &
        {
          if(coupon.isEmpty) {
            "#order span *" #> f"$$$monthlyTotal%2.2f"
          } else {
            "#order span *" #> {
              val freeMonths = coupon.map(_.freeMonths.get).openOr(0)

              if (freeMonths == 1) {
                s"First Month Free"
              } else {
                s"First ${freeMonths} months free"
              }
            }
          }
        }
      }
    }

    SHtml.makeFormsAjax andThen
    orderSummary &
    "#first-name" #> text(firstName, firstName = _) &
    "#last-name" #> text(lastName, lastName = _) &
    "#street-1" #> text(street1, street1 = _) &
    "#street-2" #> text(street2, street2 = _) &
    "#city" #> ajaxText(city, city = _) &
    "#state" #> ajaxText(state, possibleState => calculateTax(possibleState, zip)) &
    "#zip" #> ajaxText(zip, possibleZip => calculateTax(state, possibleZip)) &
    "#email" #> text(email, userEmail => email = userEmail.trim) &
    "#password" #> SHtml.password(password, userPassword => password = userPassword.trim) &
    "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
    ".checkout" #> SHtml.ajaxSubmit("Place Order", () => signup) &
    ".agreement" #> ClearNodesIf(subtotalWithDiscount == 0)
  }
}
