package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
import net.liftweb.mapper.By

import com.mypetdefense.service.PetFlowChoices
import com.mypetdefense.model._
import com.mypetdefense.util.ClearNodesIf

object CatProduct extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Cat Product") / "cat-product" >>
    petChosen >>
    catSizeChosen
}

class CatProduct extends Loggable {
  import PetFlowChoices._

  def chosenProduct = ".chosen-product *" #> {
    petProduct.is.map(_.name.toString)
  }

  val prices = Price.findAll(
    By(Price.code, "default"),
    By(Price.active, true)
  )

  def getPriceForProduct(product: String) = {
    val price = prices.filter { price =>
      val productName = price.product.obj.map(_.name.get).getOrElse("")
      productName == s"${product} Plus for Cats"
    }.headOption.map(_.price.get).getOrElse(0D)

    f"$price%2.2f"
  }

  def render = {
    def chooseProduct(name: String) = {
      val selectedProduct = for {
        petType <- petChoice.is
        petSize <- petSize.is
        product <- Product.find(
          By(Product.animalType, petType), 
          By(Product.size, petSize),
          By(Product.name, s"${name} Plus for Cats")
        )} yield product

      petProduct(selectedProduct)

      S.redirectTo(PetDetails.menu.loc.calcDefaultHref)
    }

    ".adventure-plus" #> ClearNodesIf(petSize.is == Full(AnimalSize.CatSmall)) andThen
    ".frontline-plus" #> {
      ".price *" #> getPriceForProduct("Frontline") &
      "#frontline-plus" #> SHtml.submit("Select", () => chooseProduct("Frontline"))
    } &
    ".zoguard-plus" #> {
      ".price *" #> getPriceForProduct("ZoGuard") &
      "#zoguard-plus" #> SHtml.submit("Select", () => chooseProduct("ZoGuard"))
    } &
    ".adventure-plus" #> {
      ".price *" #> getPriceForProduct("Adventure") &
      "#adventure-plus" #> SHtml.submit("Select", () => chooseProduct("Adventure"))
    }
  }
}
