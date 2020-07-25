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
  
  def render = {
    def chooseSize(product: Box[FleaTick]) = {

      S.redirectTo(CartReview.menu.loc.calcDefaultHref)
    }

    val products = FleaTick.findAll(
      By(FleaTick.name, "ZoGuard Plus for Cats"),
      NotBy(FleaTick.size, AnimalSize.CatAllSize)
    )

    val smallCat = products.find(_.size.get == AnimalSize.CatSmall)
    val mediumCat = products.find(_.size.get == AnimalSize.CatMedium)
    val largeCat = products.find(_.size.get == AnimalSize.CatLarge)


    "#small-cat" #> SHtml.submit("Select", () => chooseSize(smallCat)) &
    "#medium-cat" #> SHtml.submit("Select", () => chooseSize(mediumCat)) &
    "#large-cat" #> SHtml.submit("Select", () => chooseSize(largeCat))
  }
}
