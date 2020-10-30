package com.mypetdefense.util

import com.mypetdefense.model._
import com.mypetdefense.service.PetFlowChoices
import com.mypetdefense.service.PetFlowChoices._
import com.mypetdefense.snippet._
import com.mypetdefense.snippet.customer._
import com.mypetdefense.snippet.login._
import com.mypetdefense.snippet.shop.{TreatCheckout, TreatList}
import com.mypetdefense.snippet.signup._
import net.liftweb.common._
import net.liftweb.http.{RedirectResponse, _}
import net.liftweb.mapper.By
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util.Helpers._
import net.liftweb.util.Props

object Paths {

  val homePage: Menu.Menuable with Menu.WithSlash = Menu.i("Home") / "index"
  val halfOff: Menu.Menuable =
    Menu.i("50% Off") / "50off" >> EarlyResponse(() => applyCouponRedirect("50off"))
  val freeMonth: Menu.Menuable =
    Menu.i("100% Off") / "100off" >> EarlyResponse(() => applyCouponRedirect("100off"))
  val termsOfService: Menu.Menuable with Menu.WithSlash =
    Menu.i("Terms of Service") / "terms-of-service"
  val thanksPage: Menu.Menuable with Menu.WithSlash        = Menu.i("Thanks") / "thanks"
  val billingThanksPage: Menu.Menuable with Menu.WithSlash = Menu.i("Success!") / "update-success"
  val testimonial: Menu.Menuable = Menu.i("Review") / "testimonial" >>
    TemplateBox(() => Templates("testimonial" :: Nil))
  val pictureRelease: Menu.Menuable = Menu.i("Picture Release") / "picture" >>
    TemplateBox(() => Templates("picture" :: Nil))
  val loggedIn: If = If(
    () => SecurityContext.loggedIn_?,
    storeAndRedirect _
  )
  val adminUser: If = If(
    () => SecurityContext.admin_?,
    storeAndRedirect _
  )
  val mpdAdmin: If = If(
    () => SecurityContext.mpdAdmin_?,
    storeAndRedirect _
  )
  val agentUser: If = If(
    () => SecurityContext.agent_?,
    storeAndRedirect _
  )
  val parent: If = If(
    () => SecurityContext.parent_?,
    storeAndRedirect _
  )
  val agentOrAdmin: If = If(
    () => (SecurityContext.admin_? || SecurityContext.agent_?),
    storeAndRedirect _
  )
  val notLoggedIn: If = If(
    () => !SecurityContext.loggedIn_?,
    RedirectResponse("/logout")
  )
  val finishedCheckout: If = If(
    () => !total.is.isEmpty,
    RedirectResponse(Checkout.menu.loc.calcDefaultHref)
  )
  val petChosen: If = If(
    () => !petChoice.is.isEmpty,
    RedirectResponse(PetChoice.menu.loc.calcDefaultHref)
  )
  val completedPet: If = If(
    () => completedPets.nonEmpty,
    () => RedirectResponse(PetChoice.menu.loc.calcDefaultHref)
  )
  val createdAccount: If = If(
    () => SecurityContext.loggedIn_?,
    () => RedirectResponse(CreateAccount.menu.loc.calcDefaultHref)
  )

  def storeAndRedirect: RedirectResponse = {
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

  def applyCouponRedirect(couponCode: String): Nothing = {
    val coupon = Coupon.find(By(Coupon.couponCode, couponCode.toLowerCase()))
    println(coupon)
    PetFlowChoices.coupon(coupon)
    PetFlowChoices.coupon.is

    S.redirectTo(DogDetails.menu.loc.calcDefaultHref)
  }

  def serverUrl: String = {
    val hostUrl = Props.get("server.url") openOr "http://localhost:8080/"

    if (hostUrl.endsWith("/"))
      hostUrl.dropRight(1)
    else
      hostUrl
  }

  def siteMap: SiteMap = SiteMap(
    homePage,
    halfOff,
    freeMonth,
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
    CreateAccount.menu,
    Checkout.menu,
    Success.menu,
    AccountOverview.menu,
    UpgradeAccount.menu,
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
    admin.ExecutiveDashboard.menu,
    admin.ExecutiveDashboard.executiveSnapshotExportMenu,
    admin.ShipmentDashboard.menu,
    admin.ShipmentDashboard.newLabelsExportMenu,
    admin.ShipmentDashboard.existingLabelsExportMenu,
    admin.ShipmentDashboard.mpdShipstationExportMenu,
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
    admin.AmazonOrders.menu,
    admin.AmazonOrders.exportAmazonOrder,
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
    Login.logoutMenu,
    TreatList.treatListMenu,
    AddOnSale.addOnSaleMenu,
    TreatCheckout.menu,
    AddOnCheckout.menu,
    inventory.InventoryItems.menu,
    inventory.Reconciliations.menu,
    inventory.InventoryChangeAudits.menu,
    inventory.ItemProduction.menu,
    petland.NewOrder.menu,
    PetChoice.menu,
    DogDetails.menu
  )

  object intendedPath extends SessionVar[Box[String]](Empty)
}
