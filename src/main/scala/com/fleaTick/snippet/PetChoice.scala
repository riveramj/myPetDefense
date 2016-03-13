package com.fleaTick.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._


object PetChoice extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Pet Choice") / "pet-choice"
}

class PetChoice extends Loggable {
  object petChoice extends SessionVar[Box[String]](Empty)

  var selectedPet = ""

  val pets = SHtml.radio(Seq("Dog", "Cat"), petChoice.is, selectedPet = _).toForm

  def render = {
    def choosePet() = {
      petChoice(Full(selectedPet))
    }

    ClearClearable andThen
    ".pet-choice" #> pets.map { pet =>
      "input" #> pet
    } &
    "button" #> SHtml.onSubmitUnit(choosePet)
  }
}
