package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._

import com.mypetdefense.snippet.PetChoice._
import com.mypetdefense.model._

object PetSize extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Pet Size") / "pet-size"

  object petSize extends SessionVar[Box[AnimalSize.Value]](Empty)
}

class PetSize extends Loggable {
  import PetSize._

  var selectedSize: Box[AnimalSize.Value] = Empty

  val possiblePetSizes = 
    if (petChoice.is == Full(AnimalType.Cat))
      AnimalSize.values.toList.filter(size => (size == AnimalSize.Small) || (size == AnimalSize.Large))
    else
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

      S.redirectTo(PetProduct.menu.loc.calcDefaultHref)
    }

    ClearClearable andThen
    ".pet-size" #> petSizes.map { size =>
      "input" #> size
    } &
    "button" #> SHtml.onSubmitUnit(chooseSize)
  }
}

