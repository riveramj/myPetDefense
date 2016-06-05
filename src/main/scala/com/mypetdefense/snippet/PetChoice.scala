package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._

import com.mypetdefense.service.PetFlowChoices
import com.mypetdefense.model.AnimalType

object PetChoice extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Pet Choice") / "pet-choice"
}

class PetChoice extends Loggable {
  import PetFlowChoices._

  var selectedPet: Box[AnimalType.Value] = Empty

  def render = {
    def dogFlow = {
      petChoice(Full(AnimalType.Dog))

      DogProduct.menu.loc.calcDefaultHref
    }

    def catFlow = {
      petChoice(Full(AnimalType.Cat))

      CatSize.menu.loc.calcDefaultHref
    }

    "#dog [href]" #> dogFlow &
    "#cat [href]" #> catFlow
  }
}
