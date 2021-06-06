package com.mypetdefense.snippet.signup

import com.mypetdefense.model.AnimalSize.sizeToCategory
import com.mypetdefense.service.PetFlowChoices.cart
import net.liftweb.common._
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.http.{IdMemoizeTransform, S, SHtml}
import net.liftweb.sitemap.Menu
import net.liftweb.util.Helpers._


object CartReview extends Loggable {
  import net.liftweb.sitemap._

  val menu = Menu.i("Cart Review") / "cart-review"
}

class CartReview extends Loggable {
  var pets = cart.is
  var shoppingCartRenderer: Box[IdMemoizeTransform] = Empty

  def navTo(destination: Menu.Menuable) =
    SHtml.ajaxInvoke(() => S.redirectTo(destination.loc.calcDefaultHref))

  def removePet(petId: Long)() = {
    pets.remove(petId)
    cart(pets)

    shoppingCartRenderer.map(_.setHtml()).openOr(Noop)
  }


  def render = {
    "#shopping-cart" #> SHtml.idMemoize { renderer =>
      shoppingCartRenderer = Full(renderer)

      val subtotal = pets.values.map(_.price.price.get).sum

      ".pet" #> pets.map { case (petId, checkoutPet) =>
        val pet = checkoutPet.pendingPet.pet
        val price = checkoutPet.price

        ".pet-name-actions" #> {
          ".name *" #> pet.name.get &
          ".remove-pet [onclick]" #> SHtml.ajaxInvoke(removePet(petId))
      } &
        ".pet-plan" #> {
          ".plan *" #> s"${checkoutPet.pendingPet.boxType.toString} Protection" &
          ".plan-size *" #> s"for ${sizeToCategory(pet.size.get)} ${pet.animalType.get}s"
        } &
        ".pet-price .price *" #> f"${price.price.get}%.2f"
      } &
      ".totals" #> {
        "#subtotal .amount *" #> f"$subtotal%.2f"
      } &
      ".next-actions" #> {
        "#add-pet [onclick]" #> navTo(PetDetails.menu) &
        "#checkout [onclick]" #> navTo(Checkout.menu)
      }
    }
  }
}
