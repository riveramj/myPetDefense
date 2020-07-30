package com.mypetdefense.snippet.signup

import java.text.SimpleDateFormat

import com.mypetdefense.model._
import com.mypetdefense.service.PetFlowChoices._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import net.liftweb._
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._

object DogDetails extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu = Menu.i("Dog Details") / "dog-details" >>
    petChosen
}

class DogDetails extends Loggable {
  val formatter = new SimpleDateFormat("MM/yy")

  var currentPets = completedPets.is
  var petName = ""
  var petMonth = ""
  var petYear = ""
  var petSize: Box[AnimalSize.Value] = Empty
  var nameErrors: List[String] = Nil

  val products = FleaTick.findAll(By(FleaTick.name, "ZoGuard Plus for Dogs"))

  val smallDog = getSizeNumber(AnimalSize.DogSmallZo)
  val mediumDog = getSizeNumber(AnimalSize.DogMediumZo)
  val largeDog = getSizeNumber(AnimalSize.DogLargeZo)
  val xlargeDog = getSizeNumber(AnimalSize.DogXLargeZo)

  def getSizeNumber(size: AnimalSize.Value) = {
    products.find(_.size.get == size).map(_.size.get)
  }

  def validateNameBirthday = {
    (
      checkEmpty(petName, "#pet-name") ::
      checkEmpty(petMonth, "#pet-month") ::
      checkEmpty(petYear, "#pet-year") ::
      checkEmpty(petSize.map(_.toString), ".pet-size") ::
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
          .size(petSize.openOr(null))

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

  def goToCreateAccount() =
    validateAndNavigate(CreateAccount.menu.loc.calcDefaultHref)

  def addNewPet() =
    validateAndNavigate(PetChoice.menu.loc.calcDefaultHref)

  def chooseSize(animalSize: Option[AnimalSize.Value]) = {
    petSize = animalSize
  }

  def monthDropdown = {
    SHtml.select(
      ("","") +: List(
        "January", "February", "March", "April", "May", "June",
        "July", "August", "September", "November", "October", "November", "December"
      ).map(month => (month, month)),
      Full(petMonth),
      petMonth = _
    )
  }

  def yearDropdown = {
    SHtml.select(
      ("","") +: List(
        "2020", "2019", "2018", "2017", "2016", "2015",
        "2014", "2013", "2012", "2011", "2010", "2009",
        "2008", "2007", "2006", "2005", "2004", "2003"
      ).map(year => (year, year)),
      Full(petYear),
      petYear = _
    )
  }

  def render = {
    SHtml.makeFormsAjax andThen
    "#pet-name" #> ajaxText(petName, petName = _) &
    "#month-container #pet-month" #> monthDropdown &
    "#year-container #pet-year" #> yearDropdown &
    ".small-dog .weight-number *" #> smallDog.map(_.toString + " lb") &
    ".small-dog #small-dog [onclick]" #> ajaxInvoke( () => chooseSize(smallDog)) &
    ".medium-dog .weight-number *" #> mediumDog.map(_.toString + " lb") &
    ".medium-dog #medium-dog [onclick]" #> ajaxInvoke( () => chooseSize(mediumDog)) &
    ".large-dog .weight-number *" #> largeDog.map(_.toString + " lb") &
    ".large-dog #large-dog [onclick]" #> ajaxInvoke( () => chooseSize(largeDog)) &
    ".xlarge-dog .weight-number *" #> xlargeDog.map(_.toString + " lb") &
    ".xlarge-dog #xlarge-dog [onclick]" #> ajaxInvoke( () => chooseSize(xlargeDog)) &
    "#add-pet" #> ajaxSubmit("Add Pet", addNewPet _) &
    "#checkout" #> ajaxSubmit("Checkout", goToCreateAccount _)
  }
}
