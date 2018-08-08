package com.mypetdefense.snippet
package admin

import net.liftweb._
  import sitemap.Menu
  import http.SHtml._
  import http._
  import js.JsCmds._

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.actor._

object GrowthRates extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Growth Rates") / "admin" / "growth-rates" >>
    adminUser >>
    loggedIn
}

class GrowthRates extends Loggable {
  var breed = ""
  var mediumAge = ""
  var largeAge = ""
  var xlargeAge = ""

  val growthRates = GrowthRate.findAll()

  def createGrowthRate = {
    println("in create")
    def convertAgeToInt(age: String) = tryo(age.trim().toInt)

    GrowthRate.createGrowthRate(
      breed,
      convertAgeToInt(mediumAge),
      convertAgeToInt(largeAge),
      convertAgeToInt(xlargeAge)
    )

    S.redirectTo(GrowthRates.menu.loc.calcDefaultHref)
  }

  def deleteGrowthRate(growthRate: GrowthRate)() = {
    if (growthRate.delete_!)
      S.redirectTo(GrowthRates.menu.loc.calcDefaultHref)
    else
      Alert("An error has occured. Please try again.")
  }

  def formatGrowthMonth(growthMonth: Int) = {
    if (growthMonth == -1)
      "-"
    else
      s"$growthMonth months"
  }
  
  def render = {
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
      ".large-age *" #> formatGrowthMonth(growthRate.largeProductMonth.get)&
      ".xlarge-age *" #> formatGrowthMonth(growthRate.xlargeProductMonth.get)&
      ".actions .delete [onclick]" #> Confirm(s"Delete ${growthRate.breed}?",
        ajaxInvoke(deleteGrowthRate(growthRate) _)
      )
    }

  }
}
