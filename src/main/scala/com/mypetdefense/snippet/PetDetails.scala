package com.mypetdefense.snippet

import net.liftweb._
  import http.SHtml._
  import util._
  import util.Helpers._
  import common._
  import util.ClearClearable
  import http._
  import mapper.{By, NullRef}
  import js._
  import JsCmds._

import com.mypetdefense.service._
  import ValidationService._
  import PetFlowChoices._

import com.mypetdefense.model._

import java.util.Date
import java.text.SimpleDateFormat

object PetDetails extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Pet Details") / "pet-details" >>
    completedPetOrFlow
}

class PetDetails extends Loggable {
  val formatter = new SimpleDateFormat("MM/yy")

  var currentPets = completedPets.is

  def goToCheckout() = {
    val validateFields = Nil

    if(validateFields.isEmpty) {
      S.redirectTo(Checkout.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def addNewPet() = {
    petChoice(Empty)
    petProduct(Empty)
    petSize(Empty)
    petId(Empty)

    completedPets(currentPets)

    S.redirectTo(PetChoice.menu.loc.calcDefaultHref)
  }

  def updatePetName(name: String, pet: Pet) = {
    val updatedPet = pet.name(name)

    currentPets(pet.petId.get) = updatedPet
    completedPets(currentPets)
    
    Noop
  }

  def updatePetBirthday(birthday: String, pet: Pet) = {
    val updatedPet = tryo(formatter.parse(birthday)).map(pet.birthday(_))

    updatedPet.map( pet => currentPets(pet.petId.get) = pet)
    completedPets(currentPets)
    
    Noop
  }

  def removePet(pet: Pet)() = {
    currentPets.remove(pet.petId.get)
    completedPets(currentPets)

    if (petId.is == Full(pet.petId.get)) {
      petChoice(Empty)
      petProduct(Empty)
      petSize(Empty)
      petId(Empty)
    }
    
    if (completedPets.size == 0 && petId.isEmpty)
      S.redirectTo(PetChoice.menu.loc.calcDefaultHref)
    else
      S.redirectTo(PetDetails.menu.loc.calcDefaultHref)
  }

  def render = {
    val currentPet = {
      for {
        petType <- petChoice.is
        size <- petSize.is
        product <- petProduct.is
      } yield {
        Pet.create
          .petId(petId.is.openOr(0L))
          .animalType(petType)
          .size(size)
          .product(product)
      }
    }

    currentPet.map { pet => 
      if (currentPets.get(pet.petId.get).isEmpty)
        currentPets(pet.petId.get) = pet
    }

    completedPets(currentPets)

    SHtml.makeFormsAjax andThen
    ".pet" #> currentPets.values.map { pet =>
      var name = pet.name.get
      var birthday = tryo(formatter.format(pet.birthday.get).toString).openOr("")

      ".remove-pet [onclick]" #> ajaxInvoke(removePet(pet)) &
      ".details-pet *" #> pet.animalType.toString &
      ".details-product *" #> pet.product.obj.map(_.name.get) &
      ".details-size *" #> pet.size.toString &
      ".pet-name" #> ajaxText(name, possibleName => updatePetName(possibleName, pet)) &
      ".birthday" #> ajaxText(birthday, possibleBirthday => updatePetBirthday(possibleBirthday, pet))
    } &
    "#add-pet" #> ajaxSubmit("Add Pet", addNewPet) &
    "#checkout" #> ajaxSubmit("Checkout", goToCheckout)
  }
}
