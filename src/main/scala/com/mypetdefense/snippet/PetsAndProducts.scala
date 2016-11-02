package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
  import js.JsCmds._
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.Paths._
import com.mypetdefense.actor._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.util.SecurityContext._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service.ParentService


object PetsAndProducts extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Pets and Products") / "pets-products" >>
    loggedIn >>
    parent
}

class PetsAndProducts extends Loggable {
  val user = currentUser

  var newPetType: Box[AnimalType.Value] = Empty
  var newPetChosenProduct: Box[Product] = Empty
  var newPetName = ""

  def petTypeDropdown(renderer: IdMemoizeTransform) = {
    SHtml.ajaxSelectObj(
      List(
        (Empty, ""),
        (Full(AnimalType.Dog), AnimalType.Dog.toString),
        (Full(AnimalType.Cat), AnimalType.Cat.toString)
      ),
      Full(newPetType),
      (possiblePetType: Box[AnimalType.Value]) => {
        newPetType = possiblePetType
        renderer.setHtml
      }
    )
  }

  def productDropdown() = {
    val products = newPetType.map { animal =>
      Product.findAll(By(Product.animalType, animal))
    }.openOr(Nil)

    SHtml.ajaxSelectObj(
      (Empty, "") +: products.map(product => (Full(product), product.getNameAndSize)),
      Full(newPetChosenProduct),
      (possibleProduct: Box[Product]) => newPetChosenProduct = possibleProduct
    )
  }

  val pets = user.map { parent => 
    Pet.findAll(By(Pet.user, parent))
  }.openOr(Nil)


  def addPet = {
    val validateFields = List(
      checkEmpty(newPetName, "#new-pet-name")
    ).flatten

    if(validateFields.isEmpty) {
      (for {
        parent <- user
        pet <- newPetType
        product <- newPetChosenProduct
        size = product.size.get
      } yield {
        ParentService.addNewPet(
          user = parent,
          name = newPetName,
          animalType = pet,
          size = size,
          product = product
        )
      }).flatMap(identity) match {
        case Full(pet) =>
          S.redirectTo(PetsAndProducts.menu.loc.calcDefaultHref)
        case other =>
          Alert("An error has occured. Please try again.")
      }
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".pets-products [class+]" #> "current" &
    "#new-pet" #> idMemoize { renderer =>
      "#new-pet-name" #> ajaxText(newPetName, newPetName = _) &
      "#pet-type-select" #> petTypeDropdown(renderer) &
      "#new-pet-product-select" #> productDropdown() &
      "#add-pet" #> SHtml.ajaxSubmit("Add Pet", () => addPet)
    }
  }
}
