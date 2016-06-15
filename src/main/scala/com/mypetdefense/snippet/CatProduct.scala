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

  val menu = Menu.i("Cat Product") / "cat-product"
}

class CatProduct extends Loggable {
  import PetFlowChoices._

  def chosenProduct = "#chosen-product *" #> {
    petProduct.is.map(_.name.toString)
  }

  def render = {
    def chooseProduct(name: String) = {
      val selectedProduct = for {
        petType <- petChoice.is
        petSize <- petSize.is
        product <- Product.find(
          By(Product.animalType, petType), 
          By(Product.size, petSize),
          By(Product.name, name)
        )} yield product

      petProduct(selectedProduct)

      S.redirectTo(Checkout.menu.loc.calcDefaultHref)
    }

    ".zoguard-plus" #> ClearNodesIf(petSize.is == Full(AnimalSize.CatSmall)) andThen
    "#zoguard-plus" #> SHtml.submit("Select", () => chooseProduct("ZoGuard Plus for Cats")) &
    "#adventure-plus" #> SHtml.submit("Select", () => chooseProduct("Adventure Plus for Cats"))
  }
}
