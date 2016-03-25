package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._

import com.mypetdefense.snippet.PetChoice._
import com.mypetdefense.snippet.PetSize._
import com.mypetdefense.model._

object Plan extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Plan") / "plan"

  object plan extends SessionVar[Box[SubscriptionType.Value]](Empty)
}

class Plan extends Loggable {
  import Plan._

  var selectedPlan: Box[SubscriptionType.Value] = Empty

  val plans = 
    if (petChoice.is == Full(AnimalType.Dog))
      SHtml.radio(
        SubscriptionType.values.toList.map(_.toString), 
        plan.is.map(_.toString), 
        selected => selectedPlan = Full(SubscriptionType.withName(selected))
      ).toForm
    else
      SHtml.radio(
        SubscriptionType.values.toList.map(_.toString), 
        plan.is.map(_.toString), 
        selected => selectedPlan = Full(SubscriptionType.withName(selected))
      ).toForm

  def render = {
    def choosePlan() = {
      plan(selectedPlan)

      S.redirectTo(Checkout.menu.loc.calcDefaultHref)
    }

    ClearClearable andThen
    ".pet-plan" #> plans.map { plan =>
      "input" #> plan
    } &
    "button" #> SHtml.onSubmitUnit(choosePlan)
  }
}



