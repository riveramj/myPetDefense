package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http.{S, SHtml}

import com.mypetdefense.service.PetFlowChoices
import com.mypetdefense.model._

object CatSize extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Cat Size") / "cat-size" >>
    petChosen
}

class CatSize extends Loggable {
  import PetFlowChoices._

  def chosenSize = "#chosen-size *" #> {
    petSize.is.map(_.toString)
  }
  
  def render = {
    def chooseSize(size: AnimalSize.Value) = {
      petSize(Full(size))

      S.redirectTo(CatProduct.menu.loc.calcDefaultHref)
    }

    "#small-cat" #> SHtml.submit("Select", () => chooseSize(AnimalSize.CatSmall)) &
    "#medium-cat" #> SHtml.submit("Select", () => chooseSize(AnimalSize.CatMedium)) &
    "#large-cat" #> SHtml.submit("Select", () => chooseSize(AnimalSize.CatLarge))
  }
}


