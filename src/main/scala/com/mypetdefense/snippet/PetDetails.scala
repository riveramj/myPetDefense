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
import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date
import java.text.SimpleDateFormat

object PetDetails extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Pet Details") / "pet-details" >>
    petChosen >>  
    dogProductChosen
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

    PetFlowChoices.completedPets(currentPets)

    S.redirectTo(PetChoice.menu.loc.calcDefaultHref)
  }

  def render = {
    val currentPet = {
      for {
        petType <- petChoice.is
        size <- petSize.is
        product <- petProduct.is
      } yield {
        Pet.create
          .petId(generateLongId)
          .animalType(petType)
          .size(size)
          .product(product)
      }
    }

    currentPets ++= currentPet.toList 

    SHtml.makeFormsAjax andThen
    ".pet" #> currentPets.map { pet =>
      var name = pet.name.get
      var birthday = tryo(pet.birthday.get.toString).openOr("") 

      ".details-pet *" #> pet.animalType.toString &
      ".details-product *" #> pet.product.obj.map(_.name.get) &
      ".details-size *" #> pet.size.toString &
      ".pet-name" #> ajaxText(name, name = _) &
      ".birthday" #> ajaxText(birthday, birthday = _)
    } &
    "#add-pet" #> SHtml.ajaxSubmit("Add Pet", addNewPet) &
    "#checkout" #> SHtml.ajaxSubmit("Checkout", goToCheckout)
  }
}
