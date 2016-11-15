package com.mypetdefense.snippet

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._

import com.mypetdefense.model._
import com.mypetdefense.snippet._
import com.mypetdefense.service.PetFlowChoices

class NavBar extends Loggable {
  import PetFlowChoices._

  val isDog_? = petChoice.is == Full(AnimalType.Dog)

  def size = {
    "a [href]" #> {
      if (isDog_?) {
        DogSize.menu.loc.calcDefaultHref
      } else {
        CatSize.menu.loc.calcDefaultHref
      }
    } &
    ".chosen-size *" #> petSize.is.map(_.toString)
  }

  def product = {
    "a [href]" #> {
      if (isDog_?) {
        DogProduct.menu.loc.calcDefaultHref
      } else {
        CatProduct.menu.loc.calcDefaultHref
      }
    } &
    ".chosen-product *" #> petProduct.is.map(_.name.toString)
  }

  def petName = {
    "a [href]" #> PetDetails.menu.loc.calcDefaultHref &
    ".pet-count *" #> "1 pet"
  }
}
