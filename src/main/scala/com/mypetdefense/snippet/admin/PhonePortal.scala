package com.mypetdefense.snippet
package admin

import net.liftweb._
  import sitemap.Menu
  import http.SHtml._
  import http._
  import js.JsCmds._

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.service.ValidationService._

object PhonePortal extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Phone Portal") / "admin" / "phone-portal"
}

class PhonePortal extends Loggable {

  var petType: Box[AnimalType.Value] = Full(AnimalType.Dog)
  var chosenProduct: Box[Product] = Empty
  var petName = ""

  def petTypeRadio(renderer: IdMemoizeTransform) = {
    ajaxRadio(
      List(AnimalType.Dog, AnimalType.Cat),
      petType,
      (petSelected: AnimalType.Value) => {
        petType = Full(petSelected)
        renderer.setHtml
      }
    ).toForm
  }

  def productDropdown = {
    val products = petType.map { animal =>
      Product.findAll(By(Product.animalType, animal))
    }.openOr(Nil)

    SHtml.selectObj(
      products.map(product => (product, product.getNameAndSize)),
      chosenProduct,
      (possibleProduct: Product) => chosenProduct = Full(possibleProduct)
    )
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".phone-portal [class+]" #> "current" &
    ".account-info" #> idMemoize { renderer =>
      ".pet-name" #> ajaxText(petName, petName = _) &
      ".pet-type-select" #> petTypeRadio(renderer) &
      ".product-container .product-select" #> productDropdown
    }
  }
}
