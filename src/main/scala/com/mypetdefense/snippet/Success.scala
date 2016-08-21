package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
import net.liftweb.mapper.{By, NullRef}

import com.mypetdefense.service.PetFlowChoices._
import com.mypetdefense.model._

object Success extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

    val menu = Menu.i("Success") / "success" >>
      petChosen >>
      productChosen >>
      sizeChosen >>
      finishedCheckout
}

class Success extends Loggable {

  def render() = {
    val selectedPetType = petChoice.is
    val selectedPetSize =  petSize.is
    val selectedPetProduct = petProduct.is

    "#type span *" #> petChoice.is.map(_.toString) &
    "#size span *" #> petSize.is.map(_.toString + " pounds") &
    "#product span *" #> petProduct.is.map(_.name.get) &
    "#monthly-charge #amount *" #> total.is.map(_.toString)
  }
}
