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
  var nameErrors: List[String] = Nil

  val products = FleaTick.findAll(By(FleaTick.name, "ZoGuard Plus for Dogs"))
  val smallDog = products.find(_.size.get == AnimalSize.DogSmallZo)
  val mediumDog = products.find(_.size.get == AnimalSize.DogMediumZo)
  val largeDog = products.find(_.size.get == AnimalSize.DogLargeZo)
  val xlargeDog = products.find(_.size.get == AnimalSize.DogXLargeZo)

  def getSizeNumber(product: Option[FleaTick]) = product.map(_.size.toString)

  def validateNameBirthday = {
    (
      currentPets.values.flatMap { pet =>
        checkEmpty(pet.name.get, s"#${pet.petId.get}-name")
      } ++
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

  def removePet(pet: Pet)() = {
    currentPets.remove(pet.petId.get)
    completedPets(currentPets)

    if (petId.is == Full(pet.petId.get)) {
      petChoice(Empty)
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
      } yield {
        Pet.create
          .petId(currentPetId)
          .animalType(petType)
          .size(size)
      }
    }

    currentPet.map { pet => 
      val updatedPet = {
        if (currentPets.contains(currentPetId)) 
          pet
            .name(currentPets(currentPetId).name.get)
        else
          pet
      }

      currentPets(currentPetId) = updatedPet
    }

    completedPets(currentPets)

    SHtml.makeFormsAjax andThen
    ".pet" #> currentPets.values.map { pet =>
      var name = pet.name.get

      ".remove-pet [onclick]" #> ajaxInvoke(removePet(pet) _) &
      ".details-pet *" #> pet.animalType.toString &
      ".details-size *" #> pet.size.toString &
      ".pet-name" #> ajaxText(
        name,
        possibleName => updatePetName(possibleName, pet),
        "id" -> s"${pet.petId.get}-name"
      )
    } &
      ".small .weight-number *" #> getSizeNumber(smallDog) &
      ".small #small-dog" #> SHtml.submit("Select", () => chooseSize(smallDog)) &
      ".medium .weight-number *" #> getSizeNumber(mediumDog) &
      ".medium #medium-dog" #> SHtml.submit("Select", () => chooseSize(mediumDog)) &
      ".large .weight-number *" #> getSizeNumber(largeDog) &
      ".large #large-dog" #> SHtml.submit("Select", () => chooseSize(largeDog)) &
      ".xlarge .weight-number *" #> getSizeNumber(xlargeDog) &
      ".xlarge #xlarge-dog" #> SHtml.submit("Select", () => chooseSize(xlargeDog)) &
    "#add-pet" #> ajaxSubmit("Add Pet", addNewPet _) &
    "#checkout" #> ajaxSubmit("Checkout", goToCheckout _)
  }
}
