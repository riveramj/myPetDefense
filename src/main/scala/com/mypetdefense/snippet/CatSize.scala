package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._

import com.mypetdefense.service.PetFlowChoices
import com.mypetdefense.model._

object CatSize extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Cat Size") / "cat-size"
}

class CatSize extends Loggable {
  import PetFlowChoices._

  var selectedSize: Box[AnimalSize.Value] = Empty

  val possiblePetSizes = 
      AnimalSize.values.toList.filter(size => (size == AnimalSize.Small) || (size == AnimalSize.Large))

  val petSizes = 
    SHtml.radio(
      possiblePetSizes.map(_.toString),
      petSize.is.map(_.toString),
      selected => selectedSize = Full(AnimalSize.withName(selected)) 
    ).toForm

  def render = {
    def chooseSize() = {
      petSize(selectedSize)

      S.redirectTo(CatProduct.menu.loc.calcDefaultHref)
    }

    ClearClearable andThen
    ".pet-size" #> petSizes.map { size =>
      "input" #> size
    } &
    "button" #> SHtml.onSubmitUnit(chooseSize)
  }
}


