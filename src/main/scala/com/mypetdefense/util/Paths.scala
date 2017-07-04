package com.mypetdefense.util

import net.liftweb.common._
import net.liftweb.http.RedirectResponse
import net.liftweb.sitemap._
  import Loc._
import net.liftweb.util._
  import Helpers._
import net.liftweb.util.Props
import net.liftweb.http._

import com.mypetdefense.snippet._
import com.mypetdefense.model._
import com.mypetdefense.service.PetFlowChoices._

object Paths {

  val homePage = Menu.i("Home") / "index"

  val testimonial = Menu.i("Review") / "testimonial" >>
    TemplateBox(() => Templates("testimonial" :: Nil))
  
  val loggedIn = If(
    () => SecurityContext.loggedIn_?,
    RedirectResponse("/login")
  )

  val adminUser = If(
    () => SecurityContext.admin_?,
    RedirectResponse("/login")
  )

  val parent = If(
    () => SecurityContext.parent_?,
    RedirectResponse("/login")
  )

  val agentOrAdmin = If(
    () => (SecurityContext.admin_? || SecurityContext.agent_?),
    RedirectResponse("/login")
  )

  val notLoggedIn = If(
    () => ! SecurityContext.loggedIn_?,
    RedirectResponse("/logout")
  )

  val finishedCheckout = If(
    () => !total.is.isEmpty,
    RedirectResponse(Checkout.menu.loc.calcDefaultHref)
  )

  def serverUrl = {
    val hostUrl = Props.get("server.url") openOr "http://localhost:8080/"

    if (hostUrl.endsWith("/"))
      hostUrl.dropRight(1)
    else
      hostUrl
  }

  def siteMap = SiteMap(
    homePage,
    testimonial,
    LandingPage.fetchLanding,
    Checkout.menu,
    Success.menu,
    AccountOverview.menu,
    ShippingBilling.menu,
    PetsAndProducts.menu,
    Profile.menu,
    admin.Dashboard.menu,
    admin.Dashboard.exportMenu,
    admin.Parents.menu,
    admin.Users.menu,
    admin.Prices.menu,
    admin.Coupons.menu,
    admin.Agencies.menu,
    admin.PhonePortal.menu,
    Login.menu,
    ForgotPassword.menu,
    ResetPassword.menu,
    ResetPasswordSent.menu,
    Signup.menu,
    Products.menu,
    ProductDetail.frontlineDogsMenu,
    Login.logoutMenu
  )
}
