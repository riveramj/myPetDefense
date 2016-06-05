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

object CatProduct extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Cat Product") / "cat-product"
}

class CatProduct extends Loggable {
  import PetFlowChoices._

  var selectedProduct: Box[Product] = Empty

  val products = (
    for {
      petSize <- petSize.is
      petType <- petChoice.is
      products = Product.findAll(
          By(Product.animalType, petType), 
          By(Product.size, petSize)
        )
    } yield {
      products
    }).openOr(Nil)

  val petProducts = SHtml.radioElem(
    products.map(_.name), 
    petProduct.is.map(_.name)
  ){
    selected => {
      selectedProduct = products.filter { case product => 
        Full(product.name) == selected
      }.headOption
    }
  }.toForm

  def render = {
    def chooseProduct() = {
      petProduct(selectedProduct)

      S.redirectTo(Checkout.menu.loc.calcDefaultHref)
    }

    ClearClearable andThen
    ".pet-product" #> petProducts.map { product =>
      "input" #> product
    } &
    "button" #> SHtml.onSubmitUnit(chooseProduct)
  }
}
