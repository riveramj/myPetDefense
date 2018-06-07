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

  object intendedPath extends SessionVar[Box[String]](Empty)

  def storeAndRedirect = {
    intendedPath(S.request.map { req =>
      val path = req.path
      val pathString =
        path.wholePath match {
          // If we have an end slash and the last path element is
          // "index", that element is a virtual element added by Lift
          // and we should drop it before generating the URL.
          case indexPath if indexPath.last == "index" && path.endSlash =>
            indexPath.dropRight(1).mkString("/")
          case indexPath if indexPath.last == "home" =>
            req.uri
          case menuParam if menuParam.contains("star") =>
            req.uri
          case other =>
            other.mkString("/")
        }

      val finalPath =
        ((path.absolute ? "/" | "") +
          pathString +
          (path.endSlash ? "/" | "")).replaceAll("//", "/")

      S.queryString.map { queryString =>
        finalPath + "?" + queryString
      } openOr {
        finalPath
      }
    })
    RedirectResponse(Login.menu.loc.calcDefaultHref)
  }

  val homePage = Menu.i("Home") / "index"
  
  val thanksPage = Menu.i("Thanks") / "thanks"
  val billingThanksPage = Menu.i("Success!") / "update-success"

  val testimonial = Menu.i("Review") / "testimonial" >>
    TemplateBox(() => Templates("testimonial" :: Nil))

  val pictureRelease = Menu.i("Picture Release") / "picture" >>
    TemplateBox(() => Templates("picture" :: Nil))

  val loggedIn = If(
    () => SecurityContext.loggedIn_?,
    storeAndRedirect _
  )

  val adminUser = If(
    () => SecurityContext.admin_?,
    storeAndRedirect _
  )

  val agencyUser = If(
    () => SecurityContext.agent_?,
    storeAndRedirect _
  )

  val parent = If(
    () => SecurityContext.parent_?,
    storeAndRedirect _
  )

  val agentOrAdmin = If(
    () => (SecurityContext.admin_? || SecurityContext.agent_?),
    storeAndRedirect _
  )

  val notLoggedIn = If(
    () => ! SecurityContext.loggedIn_?,
    RedirectResponse("/logout")
  )

  val finishedCheckout = If(
    () => !total.is.isEmpty,
    RedirectResponse(Checkout.menu.loc.calcDefaultHref)
  )

  val hasProductInCart = If(
    () => !shoppingCart.is.isEmpty,
    RedirectResponse(Products.menu.loc.calcDefaultHref)
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
    thanksPage,
    billingThanksPage,
    testimonial,
    pictureRelease,
    LandingPage.landing2Free,
    LandingPage.landing3Free,
    LandingPage.cold5k,
    LandingPage.doggiePalooza,
    ValentinePicture.menu,
    LandingPage.woofstock,
    LandingPage.olympics,
    LandingPage.atlantaExpo,
    LandingPage.firstMonthFree,
    CartReview.menu,
    Checkout.menu,
    Success.menu,
    AccountOverview.menu,
    ShippingBilling.menu,
    ShippingBilling.menuBilling,
    PetsAndProducts.menu,
    ParentSubscription.menu,
    ParentSubscription.manageSubscriptionMenu,
    ParentSubscription.cancelSurveySubscriptionMenu,
    ParentSubscription.surveyCompleteSubscriptionMenu,
    admin.Dashboard.menu,
    admin.Dashboard.newLabelsExportMenu,
    admin.Dashboard.existingLabelsExportMenu,
    admin.Parents.menu,
    admin.Parents.activeParentsCsvMenu,
    admin.Users.menu,
    admin.Prices.menu,
    admin.Reviews.menu,
    admin.Coupons.menu,
    admin.Agencies.menu,
    admin.Agencies.salesDataExportMenu,
    admin.Agencies.cancellationExportMenu,
    admin.Agencies.totalSalesExportMenu,
    admin.Agencies.monthToDateExportMenu,
    admin.Agencies.mtdYtdExportMenu,
    admin.PhonePortal.menu,
    admin.Surveys.menu,
    agency.AgencyOverview.agencyMtdYtdExportMenu,
    agency.AgencyOverview.exportTPPMontSalesMenu,
    agency.AgencyOverview.menu,
    Login.menu,
    ForgotPassword.menu,
    ResetPassword.menu,
    ResetPasswordSent.menu,
    Signup.menu,
    Products.menu,
    ProductDetail.frontlineDogsMenu,
    ProductDetail.zoguardDogsMenu,
    ProductDetail.adventureDogsMenu,
    ProductDetail.sheieldtecDogsMenu,
    ProductDetail.frontlineCatsMenu,
    ProductDetail.zoguardCatsMenu,
    ProductDetail.adventureCatsMenu,
    Login.logoutMenu
  )
}
