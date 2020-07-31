package com.mypetdefense.snippet.customer

import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import com.mypetdefense.util.SecurityContext._
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._
import net.liftweb.util._


object PetsAndProducts extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu = Menu.i("Pets and Products") / "pets-products" >>
    loggedIn >>
    parent
}

class PetsAndProducts extends Loggable {
  val user = currentUser
  val subscription = user.flatMap(_.subscription.obj)
  val priceCode = subscription.map(_.priceCode.get).getOrElse("")

  var newPetType: Box[AnimalType.Value] = Empty
  var newPetChosenProduct: Box[FleaTick] = Empty
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
      FleaTick.findAll(By(FleaTick.animalType, animal))
    }.openOr(Nil)

    SHtml.ajaxSelectObj(
      (Empty, "Choose Product") +: products.map(product => (Full(product), product.getNameAndSize)),
      Full(newPetChosenProduct),
      (possibleProduct: Box[FleaTick]) => newPetChosenProduct = possibleProduct
    )
  }

  val boxes = user.flatMap { parent =>
    parent.subscription.obj.map(_.subscriptionBoxes.toList)
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

  def deletePet(pet: Box[Pet])() = {
    ParentService.removePet(user, pet) match {
      case Full(_) =>
        if (Props.mode != Props.RunModes.Pilot) {
            user.map { parent =>
              pet.map(p => EmailActor ! PetRemovedEmail(parent, p))
          }
        }

        S.redirectTo(PetsAndProducts.menu.loc.calcDefaultHref)
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def savePet(pet: Box[Pet], subscriptionBox: SubscriptionBox, name: String, updatedProduct: Box[FleaTick]) = {
    val updatedPet = (
      for {
        product <- updatedProduct
        pt <- pet
        size = product.size.get
        updatedPet = pt.name(name).size(size).saveMe
      } yield {
        subscriptionBox.fleaTick(product).saveMe
        updatedPet
      }
      )
    val updatedSubscription = currentUser.flatMap(ParentService.updateStripeSubscriptionTotal)

    updatedSubscription match {
      case Full(stripeSub) =>
        S.redirectTo(PetsAndProducts.menu.loc.calcDefaultHref)
      case _ =>
        Alert("An error has occurred. Please try again.")
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
    ".pet" #>  boxes.map { box =>
      val pet = box.pet.obj
      var currentProduct = box.fleaTick.obj
      var currentPetName = pet.map(_.name.get).openOr("")
      val product = box.fleaTick.obj

      val price = if (SubscriptionBox.possiblePrice(box) == 0D) {
        product.flatMap { item =>
          Price.getPricesByCode(item, priceCode).map(_.price.get)
        }.openOr(0D)
      } else {
        SubscriptionBox.possiblePrice(box)
      }

      val currentProductDropdown = {
        val products = pet.map(pt => FleaTick.findAll(By(FleaTick.animalType, pt.animalType.get))).openOr(Nil)

        SHtml.ajaxSelectObj(
          products.map(product => (product, product.getNameAndSize)),
          currentProduct,
          (possibleProduct: FleaTick) => currentProduct = {
            Full(possibleProduct)
          }
        )
      }

      ".pet-name" #> ajaxText(currentPetName, currentPetName = _) &
        ".price *" #> f"$$$price%2.2f" &
        ".pet-status *" #> pet.map(_.status.get.toString) &
        ".pet-product" #> currentProductDropdown &
        ".cancel [onclick]" #> Confirm(s"Remove ${currentPetName} and cancel future shipments?",
          ajaxInvoke(deletePet(pet) _)
        ) &
        ".save" #> ajaxSubmit("Save Changes", () => savePet(pet, box, currentPetName, currentProduct))
    }

  }
}
