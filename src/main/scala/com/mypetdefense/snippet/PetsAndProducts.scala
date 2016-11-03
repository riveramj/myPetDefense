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
import com.mypetdefense.service._


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
        (Empty, "Choose Pet"),
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
      (Empty, "Choose Product") +: products.map(product => (Full(product), product.getNameAndSize)),
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
        PetService.addNewPet(
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

  def deletePet(pet: Pet)() = {
    PetService.removePet(user, pet) match {
      case Full(_) =>
        S.redirectTo(PetsAndProducts.menu.loc.calcDefaultHref)
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def savePet(pet: Pet, name: String, updatedProduct: Box[Product]) = {
    println(name + " name ====")
    println(updatedProduct + " prod ====")

    (
      for {
        product <- updatedProduct
        size = product.size.get
        updatedPet = pet.product(product).name(name).size(size).saveMe
      } yield {
        updatedPet 
      }
    ) match {
      case Full(pet) =>
        println(pet)
        S.redirectTo(PetsAndProducts.menu.loc.calcDefaultHref)
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".pets-products [class+]" #> "current" &
    "#user-email *" #> user.map(_.email.get) &
    "#new-pet" #> idMemoize { renderer =>
      "#new-pet-name" #> ajaxText(newPetName, newPetName = _) &
      "#pet-type-select" #> petTypeDropdown(renderer) &
      "#new-pet-product-select" #> productDropdown() &
      "#add-pet" #> SHtml.ajaxSubmit("Add Pet", () => addPet)
    } &
    ".pet" #> pets.toSeq.sortWith(_.name.get < _.name.get).map { pet =>
      var currentProduct = pet.product.obj
      var currentPetName = pet.name.get

      val currentProductDropdown = {
        val products = Product.findAll(By(Product.animalType, pet.animalType.get))

        SHtml.ajaxSelectObj(
          products.map(product => (product, product.getNameAndSize)),
          currentProduct,
          (possibleProduct: Product) => currentProduct = {
            println(possibleProduct)
            Full(possibleProduct)
          }
        )
      }

      ".pet-name" #> ajaxText(currentPetName, currentPetName = _) &
      ".pet-product" #> currentProductDropdown &
      ".pet-status *" #> pet.status.get.toString &
      ".cancel [onclick]" #> Confirm(s"Remove ${pet.name} and cancel future shipments?",
        ajaxInvoke(deletePet(pet))
      ) &
      ".save" #> ajaxSubmit("Update", () => savePet(pet, currentPetName, currentProduct))
    }
  }
}
