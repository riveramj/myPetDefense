package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._

import com.mypetdefense.service.PetFlowChoices
import com.mypetdefense.model._

object DogSize extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Dog Size") / "dog-size"
}

class DogSize extends Loggable {
  import PetFlowChoices._

  var selectedSize: Box[AnimalSize.Value] = Empty

  val possiblePetSizes = 
      AnimalSize.values.toList

  val petSizes = 
    SHtml.radio(
      possiblePetSizes.map(_.toString),
      petSize.is.map(_.toString),
      selected => selectedSize = Full(AnimalSize.withName(selected)) 
    ).toForm

  def render = {
    def chooseSize() = {
      petSize(selectedSize)

      S.redirectTo(Checkout.menu.loc.calcDefaultHref)
    }

    ClearClearable andThen
    ".pet-size" #> petSizes.map { size =>
      "input" #> size
    } &
    "button" #> SHtml.onSubmitUnit(chooseSize)
  }
}

