package com.mypetdefense.snippet.signup

import com.mypetdefense.model.AnimalSize.sizeToCategory
import com.mypetdefense.service.PetFlowChoices.cart
import com.mypetdefense.service.TaxJarService
import com.mypetdefense.snippet.{CallableFunction, MyPetDefenseEvent}
import net.liftweb.common._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{Noop, Script}
import net.liftweb.http.{IdMemoizeTransform, S, SHtml}
import net.liftweb.json.{DefaultFormats, JValue, Serialization}
import net.liftweb.sitemap.Menu
import net.liftweb.util.Helpers._


object CartReview extends Loggable {
  import net.liftweb.sitemap._

  val menu = Menu.i("Cart Review") / "cart-review"
}

case class CartItem(label: String, amount: Int)
case class UpdateCartItems(items: List[CartItem], total: CartItem) extends MyPetDefenseEvent("update-cart-items")
case class PartialAddress(city: String, region: String, postalCode: String)

class CartReview extends Loggable {
  implicit val formats: DefaultFormats.type = DefaultFormats

  var shoppingCart = cart.is
  var shoppingCartRenderer: Box[IdMemoizeTransform] = Empty
  var partialAddress = PartialAddress("", "", "")
  var taxDue: BigDecimal = 0d
  var taxRate: BigDecimal = 0d
  var subtotal: BigDecimal = 0

  def navTo(destination: Menu.Menuable) =
    SHtml.ajaxInvoke(() => S.redirectTo(destination.loc.calcDefaultHref))

  def removePet(petId: Long)() = {
    shoppingCart.remove(petId)
    cart(shoppingCart)

    shoppingCartRenderer.map(_.setHtml()).openOr(Noop)
  }

  def updateCartWithAddress(rawPartialAddress: String): JsCmd = {
    println("info")
    println(rawPartialAddress)
    println("info")

    for {
      requestJson <- tryo(Serialization.read[JValue](rawPartialAddress)) ?~! "Invalid JSON." ~> 400
      addressJson <- tryo(requestJson.extract[PartialAddress]) ?~ "Error in address json." ~> 400
    } yield {
      partialAddress = addressJson
      calculateTax
    }

    updateCartItems
  }

  def updateSubtotal() =
    subtotal = shoppingCart.values.map(_.price.price.get).sum

  def calculateTax = {
    updateSubtotal()

    if (partialAddress.postalCode.nonEmpty) {
      val taxInfo = TaxJarService.findTaxAmountAndRate(
        partialAddress,
        subtotal
      )

      taxDue = taxInfo._1
      taxRate = taxInfo._2
    }
  }

  def updateCartItems: JsCmd = {
    updateSubtotal()

    val taxAmount = tryo((taxDue * 100).toInt).openOr(0)
    val subtotalAmount = tryo((subtotal * 100).toInt).openOrThrowException("No subtotal. Please try again or contact support.")
    val totalAmount = subtotalAmount + taxAmount

    val items = shoppingCart.values.map { checkoutPet =>
      val price = tryo((checkoutPet.price.price.get * 100).toInt).openOrThrowException("Error retrieving prices. Please try again.")
      CartItem(checkoutPet.pendingPet.boxType.toString, price)
    }.toList ++ List(CartItem("Tax", taxAmount))

    val total = CartItem("Due Today", totalAmount)

    UpdateCartItems(items, total)
  }


  def render = {
    "#shopping-cart" #> SHtml.idMemoize { renderer =>
      shoppingCartRenderer = Full(renderer)

      val subtotal = shoppingCart.values.map(_.price.price.get).sum

      S.appendJs(updateCartItems)

      ".pet" #> shoppingCart.map { case (petId, checkoutPet) =>
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
      } &
      ".update-billing-amount *" #> new CallableFunction("updateBillingAmount", updateCartWithAddress)
      ".update-billing-amount-take-2 *" #> Script()
    }
  }
}
