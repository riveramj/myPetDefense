package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.util.Helpers._
import net.liftweb.common._
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

  def render = {
    def chooseSize(size: AnimalSize.Value) = {
      petSize(Full(size))

      CatProduct.menu.loc.calcDefaultHref
    }

    "#small-cat [href]" #> chooseSize(AnimalSize.CatSmall) &
    "#medium-cat [href]" #> chooseSize(AnimalSize.CatMedium) &
    "#large-cat [href]" #> chooseSize(AnimalSize.CatLarge)
  }
}


