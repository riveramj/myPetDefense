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
  var petName = ""
  var petMonth = ""
  var petYear = ""
  var nameErrors: List[String] = Nil

  val products = FleaTick.findAll(By(FleaTick.name, "ZoGuard Plus for Dogs"))
  val smallDog = products.find(_.size.get == AnimalSize.DogSmallZo)
  val mediumDog = products.find(_.size.get == AnimalSize.DogMediumZo)
  val largeDog = products.find(_.size.get == AnimalSize.DogLargeZo)
  val xlargeDog = products.find(_.size.get == AnimalSize.DogXLargeZo)

  def getSizeNumber(product: Option[FleaTick]) = product.map(_.size.toString)

  def validateNameBirthday = {
    (
      checkEmpty(petName, "#pet-name") ::
      checkEmpty(petMonth, "#pet-month") ::
      checkEmpty(petYear, "#pet-year") ::
      Nil
    ).flatten.distinct
  }

  def validateAndNavigate(url: String) = {
    val validateFields = validateNameBirthday

    if(validateFields.isEmpty) {
      for {
        newPetId <- PetFlowChoices.petId.is
        animalType <- PetFlowChoices.petChoice.is
      } yield {
        val newPet = Pet.create
          .petId(newPetId)
          .animalType(animalType)

        currentPets(newPetId) = newPet
        completedPets(currentPets)
      }

      petChoice(Empty)
      petId(Empty)

      S.redirectTo(url)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def goToCheckout() =
    validateAndNavigate(Checkout.menu.loc.calcDefaultHref)

  def addNewPet() =
    validateAndNavigate(PetChoice.menu.loc.calcDefaultHref)

  def render = {
    SHtml.makeFormsAjax andThen
    ".pet" #> currentPets.values.map { pet =>
      var name = pet.name.get

      ".details-pet *" #> pet.animalType.toString &
      ".details-size *" #> pet.size.toString &
      ".pet-name" #> ajaxText(name, name = _)
    } &
      ".small .weight-number *" #> getSizeNumber(smallDog) &
      //".small #small-dog" #> SHtml.submit("Select", () => chooseSize(smallDog)) &
      ".medium .weight-number *" #> getSizeNumber(mediumDog) &
      //".medium #medium-dog" #> SHtml.submit("Select", () => chooseSize(mediumDog)) &
      ".large .weight-number *" #> getSizeNumber(largeDog) &
      //".large #large-dog" #> SHtml.submit("Select", () => chooseSize(largeDog)) &
      ".xlarge .weight-number *" #> getSizeNumber(xlargeDog) &
      //".xlarge #xlarge-dog" #> SHtml.submit("Select", () => chooseSize(xlargeDog)) &
    "#add-pet" #> ajaxSubmit("Add Pet", addNewPet _) &
    "#checkout" #> ajaxSubmit("Checkout", goToCheckout _)
  }
}
