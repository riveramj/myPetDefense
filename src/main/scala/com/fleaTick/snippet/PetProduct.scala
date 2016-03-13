package com.fleaTick.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._

import com.fleaTick.snippet.PetChoice._
import com.fleaTick.snippet.PetSize._

object PetProduct extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Pet Product") / "pet-product"

  object petProduct extends SessionVar[Box[String]](Empty)
}

class PetProduct extends Loggable {
  import PetProduct._

  var selectedProduct = ""

  val petProducts = 
    if (petChoice.is == Full("Dog"))
      SHtml.radio(Seq("Adventure", "Urban", "Rural"), petProduct.is, selectedProduct = _).toForm
    else
      SHtml.radio(Seq("Indoor", "Outdoor"), petProduct.is, selectedProduct = _).toForm

  def render = {
    def chooseProduct() = {
      petProduct(Full(selectedProduct))
    }

    ClearClearable andThen
    ".pet-product" #> petProducts.map { product =>
      "input" #> product
    } &
    "button" #> SHtml.onSubmitUnit(chooseProduct)
  }
}


