package com.fleaTick.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._

import com.fleaTick.snippet.PetChoice._
import com.fleaTick.snippet.PetSize._
import com.fleaTick.model._

object Plan extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Plan") / "plan"

  object plan extends SessionVar[Box[String]](Empty)
}

class Plan extends Loggable {
  import Plan._

  var selectedPlan = ""

  val plans = 
    if (petChoice.is == Full(AnimalType.Dog))
      SHtml.radio(Seq("Month to Month ($7/mo)", "Yearly ($5/mo*)"), plan.is, selectedPlan = _).toForm
    else
      SHtml.radio(Seq("Month to Month ($6/mo)", "Yearly ($4/mo*)"), plan.is, selectedPlan = _).toForm

  def render = {
    def choosePlan() = {
      plan(Full(selectedPlan))

      S.redirectTo(Checkout.menu.loc.calcDefaultHref)
    }

    ClearClearable andThen
    ".pet-plan" #> plans.map { plan =>
      "input" #> plan
    } &
    "button" #> SHtml.onSubmitUnit(choosePlan)
  }
}



