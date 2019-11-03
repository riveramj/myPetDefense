package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http.{S, SHtml}
import net.liftweb.mapper.{By,NotBy}

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

  def chosenSize = ".chosen-size *" #> {
    petSize.is.map(_.toString)
  }

  def render = {
    def chooseSize(product: Box[FleaTick]) = {
      productChoice(product)
      petSize(product.map(_.size.get))

      S.redirectTo(CartReview.menu.loc.calcDefaultHref)
    }

    val products = FleaTick.findAll(
      By(FleaTick.name, "ZoGuard Plus for Cats"),
      NotBy(FleaTick.size, AnimalSize.CatAllSize)
    )

    val smallCat = products.filter(_.size == AnimalSize.CatSmall).headOption
    val mediumCat = products.filter(_.size == AnimalSize.CatMedium).headOption
    val largeCat = products.filter(_.size == AnimalSize.CatLarge).headOption


    "#small-cat" #> SHtml.submit("Select", () => chooseSize(smallCat)) &
    "#medium-cat" #> SHtml.submit("Select", () => chooseSize(mediumCat)) &
    "#large-cat" #> SHtml.submit("Select", () => chooseSize(largeCat))
  }
}
