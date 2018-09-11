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

import com.mypetdefense._
  import model.{User, UserType, BoxOrder}
  import snippet.admin.Dashboard
  import snippet.agency.AgencyOverview
import com.mypetdefense.util.SecurityContext

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
  val email = user.map(_.email.get).openOr("")

  user.map(SecurityContext.logIn(_))

  def orderBox() = {
    val stripeId = user.map(_.stripeId.get).openOr("")
    val stripeCustomer = ParentService.getStripeCustomer(stripeId)
    val stripeCard = stripeCustomer.flatMap(_.defaultSource)
    val amountPaid = 1000L
    val taxPaid = 0L
    val quantity = 1
    
    val boxCharge = ParentService.chargeStripeCustomer(
      amountPaid + taxPaid,
      stripeCustomer.map(_.id),
      "Thanksgiving Box"
    )

    (for {
      parent <- user
      charge <- boxCharge
    } yield {
      BoxOrder.createBoxOrder(
        parent,
        charge.id.getOrElse(""),
        amountPaid.toString,
        taxPaid.toString,
        quantity
      )

      S.redirectTo(Success.menu.loc.calcDefaultHref)
    }).openOr(Noop)
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".buy-box" #> SHtml.ajaxSubmit("Place Order", () => orderBox)
  }
}
