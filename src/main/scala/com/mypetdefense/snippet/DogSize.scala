package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http.{S, SHtml}

import com.mypetdefense.service.PetFlowChoices
import com.mypetdefense.model._

object DogSize extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Dog Size") / "dog-size"
}

class DogSize extends Loggable {
  import PetFlowChoices._

  def chosenSize = "#chosen-size *" #> {
    petSize.is.map(_.toString)
  }
  
  def render = {
    def chooseSize(size: AnimalSize.Value) = {
      petSize(Full(size))

      S.redirectTo(DogProduct.menu.loc.calcDefaultHref)
    }

    "#small-dog" #> SHtml.submit("Select", () => chooseSize(AnimalSize.DogSmallZo)) &
    "#medium-dog" #> SHtml.submit("Select", () => chooseSize(AnimalSize.DogMediumZo)) &
    "#large-dog" #> SHtml.submit("Select", () => chooseSize(AnimalSize.DogLargeZo))
  }
}
