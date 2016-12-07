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
  var birthdayErrors: List[String] = Nil
  var nameErrors: List[String] = Nil

  def validateNameBirthday = {
    (
      currentPets.values.map { pet =>
      checkEmpty(pet.name.get, s"#${pet.petId.get}-name")
      }.flatten ++
      birthdayErrors.map( birthdayId => ValidationError(birthdayId, "Not a valid date format.")) ++
      nameErrors.map( nameId => ValidationError(nameId, "Required."))
    ).toList.distinct
  }

  def goToCheckout() = {
    val validateFields = validateNameBirthday

    if(validateFields.isEmpty) {
      completedPets(currentPets)

      S.redirectTo(Checkout.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def addNewPet() = {
    val validateFields = validateNameBirthday

    if(validateFields.isEmpty) {
      petChoice(Empty)
      petProduct(Empty)
      petSize(Empty)
      petId(Empty)

      completedPets(currentPets)

      S.redirectTo(PetChoice.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def updatePetName(name: String, pet: Pet) = {
    val nameId = s"#${pet.petId.get}-name"
    val nameError = checkEmpty(name, nameId)

    if (nameError.isEmpty) {
      val updatedPet = pet.name(name)
      currentPets(pet.petId.get) = updatedPet
      completedPets(currentPets)

      nameErrors = nameErrors.filter(_ != nameId)

      Noop
    } else {
      nameErrors = (nameErrors :+ nameId).distinct
      nameError.foldLeft(Noop)(_ & _)
    }
  }

  def updatePetBirthday(birthday: String, pet: Pet) = {
    val birthdayId = s"#${pet.petId.get}-birthday"
    val birthdayError = checkBirthday(birthday, formatter, birthdayId)

    if (birthdayError.isEmpty) {
      val updatedPet = tryo(formatter.parse(birthday)).map(pet.birthday(_))
      updatedPet.map( pet => currentPets(pet.petId.get) = pet)
      completedPets(currentPets)

      birthdayErrors = birthdayErrors.filter(_ != birthdayId)

      Noop
    } else {
      birthdayErrors = (birthdayErrors :+ birthdayId).distinct
      birthdayError.foldLeft(Noop)(_ & _)
    }
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
    val currentPetId = PetFlowChoices.petId.is.openOr(0L)
    val currentPet = {
      for {
        petType <- petChoice.is
        size <- petSize.is
        product <- petProduct.is
      } yield {
        Pet.create
          .petId(currentPetId)
          .animalType(petType)
          .size(size)
          .product(product)
      }
    }

    currentPet.map { pet => 
      val updatedPet = {
        if (currentPets.contains(currentPetId)) 
          pet
            .name(currentPets(currentPetId).name.get)
            .birthday(currentPets(currentPetId).birthday.get)
        else
          pet
      }

      currentPets(currentPetId) = updatedPet
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
      ".pet-name" #> ajaxText(
        name,
        possibleName => updatePetName(possibleName, pet),
        "id" -> s"${pet.petId.get}-name"
      ) &
      ".birthday" #> ajaxText(
        birthday,
        possibleBirthday => updatePetBirthday(possibleBirthday, pet),
        "id" -> s"${pet.petId.get}-birthday"
      )
    } &
    "#add-pet" #> ajaxSubmit("Add Pet", addNewPet) &
    "#checkout" #> ajaxSubmit("Checkout", goToCheckout)
  }
}
