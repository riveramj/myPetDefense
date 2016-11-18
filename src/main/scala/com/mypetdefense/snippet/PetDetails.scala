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
    petChosen >>  
    dogProductChosen
}

class PetDetails extends Loggable {
  val isDog_? = PetFlowChoices.petChoice.is == Full(AnimalType.Dog)
  
  var petName = ""
  var birthday = ""

  val formatter = new SimpleDateFormat("MM/yy")

  def goToCheckout() = {
    val validateFields = List(
      checkEmpty(petName, "#pet-name"),
      checkBirthday(birthday, formatter, "#birthday")
    ).flatten

    if(validateFields.isEmpty) {
      PetFlowChoices.petName(Full(petName))
      PetFlowChoices.birthday(tryo(formatter.parse(birthday)))
      S.redirectTo(Checkout.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".details-pet *" #> PetFlowChoices.petChoice.is.map(_.toString) &
    ".details-product *" #> PetFlowChoices.petProduct.is.map(_.name.toString) &
    ".details-size *" #> PetFlowChoices.petSize.is.map(_.toString) &
    "#pet-name" #> text(petName, petName = _) &
    "#birthday" #> text(birthday, birthday = _) &
    "#checkout" #> SHtml.ajaxSubmit("Checkout", goToCheckout)
  }
}
