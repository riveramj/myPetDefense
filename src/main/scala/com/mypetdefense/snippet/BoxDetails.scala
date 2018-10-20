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

object BoxDetails extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val thanksgivingBoxMenu = 
    Menu.param[User](
      "box-details", "box-details",
      boxSalesKey => KeyService.findUserByKey(boxSalesKey, "boxSalesKey"),
      user => user.boxSalesKey.get
    ) / "box-details" >>
    MatchWithoutCurrentValue
}

case object UseNewCard extends MyPetDefenseEvent("use-new-card")

class BoxDetails extends Loggable {
  import BoxDetails._

  val user = BoxDetails.thanksgivingBoxMenu.currentValue
  val existingUser_? =  if (user.isDefined) true else false
  var useExistingCard = true

  var email = user.map(_.email.get).openOr("")
  var firstName = user.map(_.firstName.get).openOr("")
  var lastName = user.map(_.lastName.get).openOr("")
  val address = user.flatMap(_.shippingAddress)
  var street1 = address.map(_.street1.get).openOr("")
  var street2 = address.map(_.street2.get).openOr("")
  var city = address.map(_.city.get).openOr("")
  var state = address.map(_.state.get).openOr("")
  var zip = address.map(_.zip.get).openOr("")

  var bigQuantity = 0
  var smallQuantity = 0
  val bigBoxCost = 29.99
  val smallBoxCost = 19.99

  var taxDue = 0D
  var taxRate = 0D
  var priceAdditionsRenderer: Box[IdMemoizeTransform] = None
  var billingCardRenderer: Box[IdMemoizeTransform] = None

  user.map(SecurityContext.logIn(_))

  def findSubtotal = {
    (bigQuantity * bigBoxCost) + (smallQuantity * smallBoxCost)
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

  def calculateSubtotal(boxSize: String, possibleQuantity: String) = {
    boxSize match {
      case "big" =>
        bigQuantity = tryo(possibleQuantity.toInt).openOr(0)
      case "small" =>
        smallQuantity = tryo(possibleQuantity.toInt).openOr(0)
      case _ =>
    }

    priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
  }

  def orderBox() = {
    val stripeId = user.map(_.stripeId.get).openOr("")
    val stripeCustomer = ParentService.getStripeCustomer(stripeId)
    val stripeCard = stripeCustomer.flatMap(_.defaultSource)
    val amountPaid = findSubtotal + taxDue
    
    val boxCharge = ParentService.chargeStripeCustomer(
      ((amountPaid + taxDue) * 100).toLong,
      stripeCustomer.map(_.id),
      "Thanksgiving Box"
    )

    (for {
      parent <- user
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
          parent.email.get
        else
          email
      }

      val newBoxOrder = BoxOrder.createBoxOrder(
        parent,
        firstName,
        lastName,
        realEmail,
        address,
        charge.id.getOrElse(""),
        amountPaid,
        taxDue,
        bigQuantity,
        smallQuantity
      )

      EmailActor ! BoxReceiptEmail(newBoxOrder)

      boxSalesInfo(Full((bigQuantity, smallQuantity, amountPaid)))

      S.redirectTo(Success.menu.loc.calcDefaultHref)
    }).openOr(Noop)
  }

  def useNewCard() = {
    useExistingCard = false

    UseNewCard &
    billingCardRenderer.map(_.setHtml).openOr(Noop)
  }

  def render = {
    val orderSummary = {
      "#order-summary" #> SHtml.idMemoize { renderer =>
        priceAdditionsRenderer = Full(renderer)

        val subtotal = findSubtotal

        val total = subtotal + taxDue

        "#subtotal span *" #> f"$$$subtotal%2.2f" &
        "#tax" #> ClearNodesIf(taxDue == 0D) &
        "#tax span *" #> f"$$$taxDue%2.2f" &
        "#order span *" #> f"$$$total%2.2f"
      }
    }

    SHtml.makeFormsAjax andThen
    ".big-quantity" #> ajaxText("", possibleQuantity => calculateSubtotal("big", possibleQuantity)) &
    ".small-quantity" #> ajaxText("", possibleQuantity => calculateSubtotal("small", possibleQuantity)) &
    orderSummary &
    "#first-name" #> text(firstName, firstName = _) &
    "#last-name" #> text(lastName, lastName = _) &
    "#street-1" #> text(street1, street1 = _) &
    "#street-2" #> text(street2, street2 = _) &
    "#city" #> ajaxText(city, city = _) &
    "#state" #> ajaxText(state, possibleState => calculateTax(possibleState, zip)) &
    "#zip" #> ajaxText(zip, possibleZip => calculateTax(state, possibleZip)) &
    "#email" #> text(email, userEmail => email = userEmail.trim) &
    "#email" #> {
      if (existingUser_?) {
        "^ [class+]" #> "disabled" &
        "^ [disabled]" #> "disabled"
      } else {
        "^ [class+]" #> ""
      }
    } &
    ".billing-container" #> SHtml.idMemoize { renderer =>
        billingCardRenderer = Full(renderer)

        ".form-row [class+]" #> {
          if (existingUser_? && useExistingCard)
            "hide-card"
          else
            ""
        } &
        ".existing-card" #> ClearNodesIf(!existingUser_? || !useExistingCard) &
        ".existing-card .change-card [onclick]" #> ajaxInvoke(useNewCard _)
    } &
    ".buy-box" #> ajaxSubmit("Place Order", () => orderBox)
  }
}
