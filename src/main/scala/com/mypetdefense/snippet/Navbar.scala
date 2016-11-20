package com.mypetdefense.snippet

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._

import com.mypetdefense.model._
import com.mypetdefense.snippet._
import com.mypetdefense.service.PetFlowChoices
import com.mypetdefense.util.ClearNodesIf

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

  def petCount = {
    val petCount = {
      if (completedPets.is.get(petId.is.openOr(0L)).isEmpty)
        completedPets.is.size + 1
      else
        completedPets.is.size
    }

    val firstTimeFlow_? = (petId.is.isEmpty || petChoice.is.isEmpty || petSize.is.isEmpty || petProduct.is.isEmpty) && completedPets.is.isEmpty

    ".pet-count" #> ClearNodesIf(firstTimeFlow_?) &
    ".pet-count *" #> petCount
  }

  def petName = {
    "a [href]" #> PetDetails.menu.loc.calcDefaultHref
  }
}
