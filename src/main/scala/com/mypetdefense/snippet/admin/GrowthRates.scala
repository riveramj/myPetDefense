package com.mypetdefense.snippet
package admin

import com.mypetdefense.model._
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

object GrowthRates extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Growth Rates") / "admin" / "growth-rates" >>
    mpdAdmin >>
    loggedIn
}

class GrowthRates extends Loggable {
  var breed     = ""
  var mediumAge = ""
  var largeAge  = ""
  var xlargeAge = ""

  val growthRates: List[GrowthRate] = GrowthRate.findAll()

  def createGrowthRate: Nothing = {
    def convertAgeToInt(age: String) = tryo(age.trim().toInt)

    GrowthRate.createGrowthRate(
      breed,
      convertAgeToInt(mediumAge),
      convertAgeToInt(largeAge),
      convertAgeToInt(xlargeAge)
    )

    S.redirectTo(GrowthRates.menu.loc.calcDefaultHref)
  }

  def deleteGrowthRate(growthRate: GrowthRate)(): Alert = {
    if (growthRate.delete_!)
      S.redirectTo(GrowthRates.menu.loc.calcDefaultHref)
    else
      Alert("An error has occured. Please try again.")
  }

  def formatGrowthMonth(growthMonth: Int): String = {
    if (growthMonth == -1)
      "-"
    else
      s"$growthMonth months"
  }

  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
      ".create" #> {
        "#breed" #> text(breed, breed = _) &
          "#medium-age" #> text(mediumAge, mediumAge = _) &
          "#large-age" #> text(largeAge, largeAge = _) &
          "#xlarge-age" #> text(xlargeAge, xlargeAge = _) &
          "#create-item" #> SHtml.ajaxSubmit("Create Growth Rate", () => createGrowthRate)
      } &
        ".growth-rate [class+]" #> "current" &
        ".growth-rate" #> growthRates.map { growthRate =>
          ".breed *" #> growthRate.breed.get &
            ".medium-age *" #> formatGrowthMonth(growthRate.mediumProductMonth.get) &
            ".large-age *" #> formatGrowthMonth(growthRate.largeProductMonth.get) &
            ".xlarge-age *" #> formatGrowthMonth(growthRate.xlargeProductMonth.get) &
            ".actions .delete [onclick]" #> Confirm(
              s"Delete ${growthRate.breed}?",
              ajaxInvoke(deleteGrowthRate(growthRate) _)
            )
        }

  }
}
