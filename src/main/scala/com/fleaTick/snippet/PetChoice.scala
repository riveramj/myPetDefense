package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._

import com.mypetdefense.model.AnimalType

object PetChoice extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Pet Choice") / "pet-choice"

  object petChoice extends SessionVar[Box[AnimalType.Value]](Empty)
}

class PetChoice extends Loggable {
  import PetChoice._

  var selectedPet: Box[AnimalType.Value] = Empty

  val pets = 
    SHtml.radio(
      AnimalType.values.toList.map(_.toString), 
      petChoice.is.map(_.toString),
      selected => selectedPet = Full(AnimalType.withName(selected)) 
    ).toForm

  def render = {
    def choosePet() = {
      petChoice(selectedPet)

      S.redirectTo(PetSize.menu.loc.calcDefaultHref)
    }

    ClearClearable andThen
    ".pet-choice" #> pets.map { pet =>
      "input" #> pet
    } &
    "button" #> SHtml.onSubmitUnit(choosePet)
  }
}
