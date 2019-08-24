package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mapper.By

import com.mypetdefense.service._
import com.mypetdefense.model.{AnimalType, AnimalSize, Product}
import com.mypetdefense.util.RandomIdGenerator._

object PetChoice extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Pet Choice") / "pet-choice"
}

class PetChoice extends Loggable {
  import PetFlowChoices._

  def chosenPet = "*" #> {
    petChoice.is.map(_.toString)
  }

  def render = {
    def dogFlow = {
      petChoice(Full(AnimalType.Dog))
      petSize(Empty)

      if (petId.is.isEmpty)
        petId(Full(generateLongId))

      S.redirectTo(DogSize.menu.loc.calcDefaultHref)
    }

    def catFlow = {
      val catProduct = Product.find(
        By(Product.name, "ZoGuard Plus for Cats"),
        By(Product.size, AnimalSize.CatAllSize)
      )
      
      petChoice(Full(AnimalType.Cat))
      productChoice(catProduct)
      petSize(Full(AnimalSize.CatAllSize))

      if (petId.is.isEmpty)
        petId(Full(generateLongId))

      S.redirectTo(CartReview.menu.loc.calcDefaultHref)
    }

    "#dog" #> SHtml.submit("Select", dogFlow _) &
    "#cat" #> SHtml.submit("Select", catFlow _)
  }
}
