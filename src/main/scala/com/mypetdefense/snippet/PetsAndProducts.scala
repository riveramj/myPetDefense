package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util._
  import Helpers._
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
  val subscription = user.flatMap(_.getSubscription)
  val priceCode = subscription.map(_.priceCode.get).getOrElse("")

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
    Pet.findAll(
      By(Pet.user, parent),
      By(Pet.status, Status.Active)
    )
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
          oldUser = parent,
          name = newPetName,
          animalType = pet,
          size = size,
          product = product
        )
      }).flatMap(identity) match {
        case Full(pet) =>

          if (Props.mode != Props.RunModes.Pilot) {
            user.map { parent =>
              EmailActor ! NewPetAddedEmail(parent, pet)
            }
          }
    
          S.redirectTo(PetsAndProducts.menu.loc.calcDefaultHref)
        case other =>
          Alert("An error has occured. Please try again.")
      }
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def deletePet(pet: Pet)() = {
    ParentService.removePet(user, pet) match {
      case Full(_) =>
        if (Props.mode != Props.RunModes.Pilot) {
            user.map { parent =>
              EmailActor ! PetRemovedEmail(parent, pet)
          }
        }

        S.redirectTo(PetsAndProducts.menu.loc.calcDefaultHref)
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def savePet(pet: Pet, name: String, updatedProduct: Box[Product]) = {
    val updatedPet = (
      for {
        product <- updatedProduct
        size = product.size.get
        updatedPet = pet.product(product).name(name).size(size).saveMe
      } yield {
        updatedPet 
      }
    )

    val updatedSubscription = currentUser.flatMap(ParentService.updateStripeSubscriptionTotal(_))

    updatedSubscription match {
      case Full(stripeSub) =>
        S.redirectTo(PetsAndProducts.menu.loc.calcDefaultHref)
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".pets-products a [class+]" #> "current" &
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

      val priceItem = currentProduct.flatMap { item =>
        Price.getPricesByCode(item, priceCode)
      }
      val price = priceItem.map(_.price.get).getOrElse(0D)

      val currentProductDropdown = {
        val products = Product.findAll(By(Product.animalType, pet.animalType.get))

        SHtml.ajaxSelectObj(
          products.map(product => (product, product.getNameAndSize)),
          currentProduct,
          (possibleProduct: Product) => currentProduct = {
            Full(possibleProduct)
          }
        )
      }

      ".pet-name" #> ajaxText(currentPetName, currentPetName = _) &
      ".pet-product" #> currentProductDropdown &
      ".price *" #> f"$$$price%2.2f" &
      ".pet-status *" #> pet.status.get.toString &
      ".cancel [onclick]" #> Confirm(s"Remove ${pet.name} and cancel future shipments?",
        ajaxInvoke(deletePet(pet))
      ) &
      ".save" #> ajaxSubmit("Update", () => savePet(pet, currentPetName, currentProduct))
    }
  }
}
