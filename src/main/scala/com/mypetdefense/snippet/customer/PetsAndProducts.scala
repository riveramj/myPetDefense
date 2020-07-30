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

  def savePet(pet: Pet, name: String) = {
    val updatedSubscription = currentUser.flatMap(ParentService.updateStripeSubscriptionTotal)

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
      var currentPetName = pet.name.get

      ".pet-name" #> ajaxText(currentPetName, currentPetName = _) &
      ".price *" #> "$0.9999999" &
      ".pet-status *" #> pet.status.get.toString &
      ".cancel [onclick]" #> Confirm(s"Remove ${pet.name} and cancel future shipments?",
        ajaxInvoke(deletePet(pet) _)
      ) &
      ".save" #> ajaxSubmit("Save Changes", () => savePet(pet, currentPetName))
    }
  }
}
