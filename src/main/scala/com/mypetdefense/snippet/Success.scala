package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
import net.liftweb.mapper.{By, NullRef}

import com.mypetdefense.service.PetFlowChoices._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.model._

import scala.collection.mutable.LinkedHashMap

object Success extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

    val menu = Menu.i("Success") / "success" >>
      completedPetOrFlow >>
      finishedCheckout
}

class Success extends Loggable {
  def render() = {
    val pets = completedPets.is
    val monthylTotal = total.is
    val freeMonthCount = freeMonths.is.openOr(0)

    petId(Empty)
    petChoice(Empty)
    petSize(Empty)
    petProduct(Empty)
    total(Empty)
    freeMonths(Empty)
    completedPets(LinkedHashMap.empty)

    "#count span *" #> pets.size &
    "#monthly-total" #> ClearNodesIf(freeMonthCount == 0) &
    "#monthly-total span *" #> monthylTotal.map( paid => f"$$$paid%2.2f" ) &
    {
      if (freeMonthCount == 0) {
        "#checkout-total #amount *" #> monthylTotal.map( paid => f"$$$paid%2.2f" )
      } else if (freeMonthCount == 1) {
        "#checkout-total *" #> s"First Month Free"
      } else {
        "#checkout-total *" #> s"First ${freeMonthCount} months free"
      }
    }
  }
}
