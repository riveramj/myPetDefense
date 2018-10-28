package com.mypetdefense.snippet 
package inventory

import net.liftweb._
  import sitemap.Menu
  import http.SHtml._
  import http._
  import js.JsCmds._

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearNodes
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.actor._

object Skus extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Skus") / "inventory" / "skus" >>
    adminUser >>
    loggedIn
}

class Skus extends Loggable {
  val skus = Sku.findAll()
  
  var newSku = ""
  var newDescription = ""
  var newCount = ""

  var skuRenderer: Box[IdMemoizeTransform] = Empty
  var currentSku: Box[Sku] = Empty
  
  def createSku = {
    val validateFields = List(
      checkEmpty(newSku, "#new-sku"),
      checkEmpty(newDescription, "#new-description"),
      validNumber(newCount, "#new-count")
    ).flatten

    if(validateFields.isEmpty) {
      val realCount = tryo(newCount.toInt).openOr(0)

      Sku.createNewSku(newSku, newDescription, realCount)

      S.redirectTo(Skus.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def deleteSku(sku: Sku)() = {
    if (sku.delete_!)
      S.redirectTo(Skus.menu.loc.calcDefaultHref)
    else
      Alert("An error has occured. Please try again.")
  }

  def updateSku(partValue: String, partName: String, sku: Sku) = {
    var badCount = false

    partName match {
      case "sku" => sku.sku(partValue).saveMe
      case "description" => sku.description(partValue).saveMe
      case "count" =>
        val realCount = tryo(partValue.toInt)

        if (realCount.isEmpty)
          badCount = true
        else 
          sku.total(realCount.openOr(0)).saveMe
      case _ => Full(sku)
    }

    if (badCount) {
      Alert("Count is not valid. Data not saved")
    }
    else
      Noop
  }

  def skuDetailBindings(detailsRenderer: IdMemoizeTransform, sku: Sku) = {
    val actualSku = sku.sku.get
    val description = sku.description.get
    val count = sku.total.toString

    ".change-sku" #> ajaxText(actualSku, possibleSku => updateSku(possibleSku, "sku", sku)) &
      ".change-description" #> ajaxText(description, possibleDescription => updateSku(possibleDescription, "description", sku)) &
      ".change-count" #> ajaxText(count, possibleCount => updateSku(possibleCount, "count", sku))
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".skus [class+]" #> "current" &
    ".create" #> idMemoize { renderer =>
      "#new-sku" #> ajaxText(newSku, newSku = _) &
      "#new-description" #> ajaxText(newDescription, newDescription = _) &
      "#new-count" #> ajaxText(newCount, newCount = _) &
      "#create-item" #> SHtml.ajaxSubmit("Create Sku", () => createSku)
    } &
    "tbody" #> skus.sortWith(_.sku.get < _.sku.get).map { sku =>
      idMemoize { detailsRenderer =>
        ".sku-entry" #> {
          ".sku *" #> sku.sku.get &
          ".description *" #> sku.description.get &
          ".count *" #> sku.total.get &
          "^ [onclick]" #> ajaxInvoke(() => {
            if (currentSku.isEmpty) {
              currentSku = Full(sku)
            } else {
              currentSku = Empty
            }

            detailsRenderer.setHtml
          })
        } &
        ".info [class+]" #> {if (currentSku.isEmpty) "" else "expanded"} &
        "^ [class+]" #> {if (currentSku.isEmpty) "" else "expanded"} &
        ".sku-info" #> {
          if (!currentSku.isEmpty) {
            skuDetailBindings(detailsRenderer, sku)
          }
          else {
            "^" #> ClearNodes
          }
        }
      }
      //".actions .delete" #> ClearNodesIf(user.userType == UserType.Parent) &
      //".actions .delete [onclick]" #> Confirm(s"Delete ${user.name}?",
      //  ajaxInvoke(deleteUser(user) _)
      //)
    }
  }
}
