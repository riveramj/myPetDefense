package com.mypetdefense.util

import net.liftweb.common._
import net.liftweb.http.RedirectResponse
import net.liftweb.sitemap._
  import Loc._
import net.liftweb.util.Props

import com.mypetdefense.snippet._
import com.mypetdefense.model._
import com.mypetdefense.service.PetFlowChoices._

object Paths {

  val index = Menu.i("Home") / "index"

  def flowComplete_? = (!petChoice.is.isEmpty && !petSize.is.isEmpty && !petProduct.is.isEmpty)

  val petChosen = If(
    () => !petChoice.is.isEmpty,
    RedirectResponse(PetChoice.menu.loc.calcDefaultHref)
  )

  val catProductChosen = If(
    () => !petProduct.is.isEmpty,
    RedirectResponse(CatProduct.menu.loc.calcDefaultHref)
  )

  val dogProductChosen = If(
    () => !petProduct.is.isEmpty,
    RedirectResponse(DogProduct.menu.loc.calcDefaultHref)
  )

  val catSizeChosen = If(
    () => !petSize.is.isEmpty,
    RedirectResponse(CatSize.menu.loc.calcDefaultHref)
  )

  val dogSizeChosen = If(
    () => !petSize.is.isEmpty,
    RedirectResponse(DogSize.menu.loc.calcDefaultHref)
  )

  val productChosen = If(
    () => !petProduct.is.isEmpty,
    () => {
      petChoice.is match {
        case Full(AnimalType.Dog) => 
          RedirectResponse(DogProduct.menu.loc.calcDefaultHref)
        case Full(AnimalType.Cat) => 
          RedirectResponse(CatProduct.menu.loc.calcDefaultHref)
        case _ => 
          RedirectResponse(PetChoice.menu.loc.calcDefaultHref)
      }
    }
  )

  val sizeChosen = If(
    () => !petSize.is.isEmpty, 
    () => {
      petChoice.is match {
        case Full(AnimalType.Dog) => 
          RedirectResponse(DogSize.menu.loc.calcDefaultHref)
        case Full(AnimalType.Cat) => 
          RedirectResponse(CatSize.menu.loc.calcDefaultHref)
        case _ => 
          RedirectResponse(PetChoice.menu.loc.calcDefaultHref)
      }
    }
  )

  val completedPetOrFlow = If(
    () => (!completedPets.isEmpty || flowComplete_?),
    () => RedirectResponse(PetChoice.menu.loc.calcDefaultHref)
  )

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
    index,
    PetChoice.menu,
    DogSize.menu,
    PetDetails.menu,
    CatSize.menu,
    DogProduct.menu,
    CatProduct.menu,
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
    admin.Coupons.menu,
    admin.Agencies.menu,
    admin.PhonePortal.menu,
    Login.menu,
    ForgotPassword.menu,
    ResetPassword.menu,
    ResetPasswordSent.menu,
    Signup.menu,
    Login.logoutMenu
  )
}
