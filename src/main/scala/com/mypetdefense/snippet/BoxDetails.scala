package com.mypetdefense.snippet

import net.liftweb._
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
  import model.{User, UserType, BoxOrder}
  import snippet.admin.Dashboard
  import snippet.agency.AgencyOverview
import com.mypetdefense.util.SecurityContext
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
    MatchWithoutCurrentValue >>
    IfValue(_.isDefined, ()=> {
      RedirectResponse(Login.menu.loc.calcDefaultHref)
    })
}

class BoxDetails extends Loggable {
  import BoxDetails._

  val user = BoxDetails.thanksgivingBoxMenu.currentValue
  val address = user.flatMap(_.shippingAddress)
  val city = address.map(_.city.get).openOr("")
  val state = address.map(_.state.get).openOr("")
  val zip = address.map(_.zip.get).openOr("")

  val email = user.map(_.email.get).openOr("")

  var quantity = 1
  val boxCost = 19.99

  var taxDue = 0D
  var taxRate = 0D

  user.map(SecurityContext.logIn(_))

  def calculateTax = {
    val taxInfo = TaxJarService.findTaxAmoutAndRate(
      city,
      state,
      zip,
      (quantity * boxCost)
    )

    taxDue = taxInfo._1
    taxRate = taxInfo._2

    //priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
  }

  def orderBox() = {
    val stripeId = user.map(_.stripeId.get).openOr("")
    val stripeCustomer = ParentService.getStripeCustomer(stripeId)
    val stripeCard = stripeCustomer.flatMap(_.defaultSource)
    val amountPaid = quantity * boxCost + taxDue
    
    val boxCharge = ParentService.chargeStripeCustomer(
      ((amountPaid + taxDue) * 100).toLong,
      stripeCustomer.map(_.id),
      "Thanksgiving Box"
    )

    (for {
      parent <- user
      charge <- boxCharge
      address <- parent.shippingAddress
    } yield {
      val newBoxOrder = BoxOrder.createBoxOrder(
        parent,
        parent.firstName.get,
        parent.lastName.get,
        parent.email.get,
        address,
        charge.id.getOrElse(""),
        amountPaid,
        taxDue,
        quantity
      )

      EmailActor ! BoxReceiptEmail(newBoxOrder)

      boxSalesInfo(Full(quantity, amountPaid))

      S.redirectTo(Success.menu.loc.calcDefaultHref)
    }).openOr(Noop)
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".quantity *" #> SHtml.text("1", possibleQuanity => quantity = tryo(possibleQuanity.toInt).openOr(0)) &
    ".buy-box" #> SHtml.ajaxSubmit("Place Order", () => orderBox)
  }
}
