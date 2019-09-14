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

  def flowComplete_? = (!petChoice.is.isEmpty && !petSize.is.isEmpty && !productChoice.is.isEmpty)  

  val homePage = Menu.i("Home") / "index"
  val termsOfService = Menu.i("Terms of Service") / "terms-of-service"
  
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

  val mpdAdmin = If(
    () => SecurityContext.mpdAdmin_?,
    storeAndRedirect _
  )

  val agentUser = If(
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

  val petChosen = If(
    () => !petChoice.is.isEmpty,
    RedirectResponse(PetChoice.menu.loc.calcDefaultHref)
  )

  val sizeChosen = If(
    () => !petSize.is.isEmpty,
    if (petChoice.is == Full(AnimalType.Dog))
      RedirectResponse(DogSize.menu.loc.calcDefaultHref)
    else
      RedirectResponse(CatSize.menu.loc.calcDefaultHref)
  )

  val completedPetOrFlow = If(
    () => (!completedPets.isEmpty || flowComplete_?),
    () => RedirectResponse(PetChoice.menu.loc.calcDefaultHref)
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
    termsOfService,
    thanksPage,
    billingThanksPage,
    testimonial,
    pictureRelease,
    LandingPage.landing2Free,
    LandingPage.landing3Free,
    LandingPage.cold5k,
    LandingPage.doggiePalooza,
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
    ParentSubscription.confirmCancelMenu,
    ParentSubscription.confirmPauseMenu,
    ParentSubscription.successfulPauseMenu,
    ParentSubscription.confirmResumeMenu,
    ParentSubscription.successfulResumeMenu,
    ParentSubscription.cancelSurveySubscriptionMenu,
    ParentSubscription.surveyCompleteSubscriptionMenu,
    admin.Dashboard.menu,
    admin.Dashboard.newLabelsExportMenu,
    admin.Dashboard.existingLabelsExportMenu,
    admin.Dashboard.mpdShipstationExportMenu,
    admin.Parents.menu,
    admin.Users.menu,
    admin.GrowthRates.menu,
    admin.Prices.menu,
    admin.Reviews.menu,
    admin.Coupons.menu,
    admin.Reporting.menu,
    admin.Agencies.menu,
    admin.Agencies.salesDataExportMenu,
    admin.Agencies.cancellationExportMenu,
    admin.Agencies.totalSalesExportMenu,
    admin.Agencies.sameDayCancelExportMenu,
    admin.Agencies.monthToDateExportMenu,
    admin.Agencies.mtdYtdExportMenu,
    admin.PhonePortal.menu,
    admin.Surveys.menu,
    admin.EventsDashboard.menu,
    agency.LegacyAgencyOverview.agencyMtdYtdExportMenu,
    agency.LegacyAgencyOverview.exportTPPMontSalesMenu,
    agency.LegacyAgencyOverview.menu,
    agency.AgencyOverview.menu,
    agency.AgencyOverview.exportAgencyCustomerMenu,
    Login.menu,
    ForgotPassword.menu,
    ResetPassword.menu,
    ResetPasswordSent.menu,
    Signup.menu,
    Products.menu,
    ProductDetail.zoguardDogsMenu,
    ProductDetail.adventureDogsMenu,
    ProductDetail.sheieldtecDogsMenu,
    ProductDetail.zoguardCatsMenu,
    ProductDetail.adventureCatsMenu,
    Login.logoutMenu,
    friendsfamily.Dashboard.menu,
    friendsfamily.Dashboard.friendsFamilyLabelsExportMenu,
    friendsfamily.Orders.menu,
    friendsfamily.Products.menu,
    TreatList.treatListMenu,
    TreatCheckout.menu,
    inventory.InventoryItems.menu,
    inventory.Reconciliations.menu,
    inventory.InventoryChangeAudits.menu,
    inventory.ItemProduction.menu,
    petland.NewOrder.menu,
    PetChoice.menu,
    DogSize.menu,
    CatSize.menu,
    PetDetails.menu,
  )
}
