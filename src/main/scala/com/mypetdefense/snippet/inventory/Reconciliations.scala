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

object Reconciliations extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Reconciliations") / "inventory" / "reconciliations" >>
    mpdAdmin >>
    loggedIn
}

class Reconciliations extends Loggable {
  val reconciliationDateFormat = new java.text.SimpleDateFormat("M/d/y")

  val reconciliations = ReconciliationEvent.findAll()
  
  var newDate = ""
  var chosenItem: Box[InventoryItem] = Empty

  var reconciliationRenderer: Box[IdMemoizeTransform] = Empty
  var currentReconciliation: Box[ReconciliationEvent] = Empty

  var actualItemCount = ""
  
  def createReconciliationEvent = {
    val validateFields = List(
      validDate(newDate, reconciliationDateFormat, "#new-date")
    ).flatten

    if(validateFields.isEmpty) {
      val realDate = reconciliationDateFormat.parse(newDate)

      ReconciliationEvent.createNewReconciliationEvent(realDate)

      S.redirectTo(Reconciliations.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def deleteReconciliation(reconciliation: ReconciliationEvent)() = {
    if (reconciliation.delete_!)
      S.redirectTo(Reconciliations.menu.loc.calcDefaultHref)
    else
      Alert("An error has occured. Please try again.")
  }

  def inventoryItemDropdown = {
    val inventoryItems = InventoryItem.findAll()

    SHtml.ajaxSelectObj(
      List((Empty,"")) ::: inventoryItems.map(item => (Full(item), item.description.get)),
      Full(chosenItem),
      (possibleItem: Box[InventoryItem]) => chosenItem = possibleItem
    )
  }

  def addItemToReconciliation(
    reconciliationEvent: ReconciliationEvent,
    renderer: IdMemoizeTransform
  )() = {
    chosenItem.map { item =>

      val expectedCount = item.total.get
      val realCount = tryo(actualItemCount.toInt).openOr(0)

      ItemReconciliation.createNewItemReconciliation(item, reconciliationEvent, realCount, expectedCount)

      chosenItem = Empty
      actualItemCount = ""

      renderer.setHtml
    }.openOr(Noop)
  }

  def reconciliationDetailBindings(
    detailsRenderer: IdMemoizeTransform,
    reconciliation: ReconciliationEvent
  ) = {
    val itemReconciliations = ItemReconciliation.findAll(By(ItemReconciliation.reconciliationEvent, reconciliation))
    

    ".item-reconciliation" #> itemReconciliations.sortWith(_.inventoryItem.obj.map(_.itemNumber.get).openOr("") < _.inventoryItem.obj.map(_.itemNumber.get).openOr("")).map { itemReconciliation =>
      val item = itemReconciliation.inventoryItem.obj

      ".item-number *" #> item.map(_.itemNumber.get) &
      ".description *" #> item.map(_.description.get) &
      ".expected-count *" #> itemReconciliation.expectedCount.get &
      ".actual-count *" #> itemReconciliation.actualCount.get
    } &
    ".add-item-container" #> {
      ".item-select" #> inventoryItemDropdown &
      ".actual-item-count" #> ajaxText(actualItemCount, actualItemCount = _) &
      ".create-item-container .create-item [onclick]" #> SHtml.ajaxInvoke(addItemToReconciliation(reconciliation, detailsRenderer) _)
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".reconciliation [class+]" #> "current" &
    ".create" #> idMemoize { renderer =>
      "#new-date" #> ajaxText(newDate, newDate = _) &
      "#create-item" #> SHtml.ajaxSubmit("Create Reconciliation", () => createReconciliationEvent)
    } &
    "tbody" #> reconciliations.sortWith(_.eventDate.get.getTime > _.eventDate.get.getTime).map { reconciliation =>
      idMemoize { detailsRenderer =>
        ".reconciliation-entry" #> {
          ".date *" #> reconciliationDateFormat.format(reconciliation.eventDate.get) &
          "^ [onclick]" #> ajaxInvoke(() => {
            if (currentReconciliation.isEmpty) {
              currentReconciliation = Full(reconciliation)
            } else {
              currentReconciliation = Empty
            }

            detailsRenderer.setHtml
          })
        } &
        ".info [class+]" #> {if (currentReconciliation.isEmpty) "" else "expanded"} &
        "^ [class+]" #> {if (currentReconciliation.isEmpty) "" else "expanded"} &
        ".reconciliation-info" #> {
          if (!currentReconciliation.isEmpty) {
            reconciliationDetailBindings(detailsRenderer, reconciliation)
          }
          else {
            "^" #> ClearNodes
          }
        }
      }
    }
  }
}
