package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http.{S, SHtml}
import net.liftweb.mapper.By

import com.mypetdefense.service.PetFlowChoices
import com.mypetdefense.model._

object DogSize extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Dog Size") / "dog-size" >>
    petChosen
}

class DogSize extends Loggable {
  import PetFlowChoices._

  def chosenSize = ".chosen-size *" #> {
    petSize.is.map(_.toString)
  }

  def render = {
    val products = FleaTick.findAll(By(FleaTick.name, "ZoGuard Plus for Dogs"))

    val smallDog = products.find(_.size.get == AnimalSize.DogSmallZo)
    val mediumDog = products.find(_.size.get == AnimalSize.DogMediumZo)
    val largeDog = products.find(_.size.get == AnimalSize.DogLargeZo)
    val xlargeDog = products.find(_.size.get == AnimalSize.DogXLargeZo)
     
    def chooseSize(product: Box[FleaTick]) = {
      petSize(product.map(_.size.get))

      S.redirectTo(CartReview.menu.loc.calcDefaultHref)
    }

    def getSizeNumber(product: Option[FleaTick]) = product.map(_.size.toString)

    ".small .weight-number *" #> getSizeNumber(smallDog) &
    ".small #small-dog" #> SHtml.submit("Select", () => chooseSize(smallDog)) &
    ".medium .weight-number *" #> getSizeNumber(mediumDog) &
    ".medium #medium-dog" #> SHtml.submit("Select", () => chooseSize(mediumDog)) &
    ".large .weight-number *" #> getSizeNumber(largeDog) &
    ".large #large-dog" #> SHtml.submit("Select", () => chooseSize(largeDog)) &
    ".xlarge .weight-number *" #> getSizeNumber(xlargeDog) &
    ".xlarge #xlarge-dog" #> SHtml.submit("Select", () => chooseSize(xlargeDog))
  }
}
