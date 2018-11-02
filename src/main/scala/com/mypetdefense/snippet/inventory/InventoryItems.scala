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
import com.mypetdefense.service.InventoryService
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.actor._

object InventoryItems extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Inventory Items") / "inventory" / "items" >>
    adminUser >>
    loggedIn
}

class InventoryItems extends Loggable {
  val inventoryItems = InventoryItem.findAll()
  
  var newItemNumber = ""
  var newDescription = ""
  var newCount = ""
  var chosenUnit: Box[UnitOfMeasure.Value] = Empty

  var itemRenderer: Box[IdMemoizeTransform] = Empty
  var currentItem: Box[InventoryItem] = Empty
  
  def createItem = {
    val validateFields = List(
      checkEmpty(newItemNumber, "#new-item-number"),
      checkEmpty(newDescription, "#new-description"),
      validNumber(newCount, "#new-count")
    ).flatten

    if(validateFields.isEmpty) {
      val realCount = tryo(newCount.toInt).openOr(0)

      chosenUnit.map { unit =>
        val newItem = InventoryItem.createNewInventoryItem(newItemNumber, newDescription, realCount, unit)

        InventoryChangeAudit.newChangeAudit(
          inventoryItem = newItem,
          newItemNumber = newItemNumber,
          newCount = realCount,
          newDescription = newDescription,
          newUnitOfMeasure = unit.toString
        )
      }

      S.redirectTo(InventoryItems.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def deleteItem(item: InventoryItem)() = {
    if (item.delete_!)
      S.redirectTo(InventoryItems.menu.loc.calcDefaultHref)
    else
      Alert("An error has occured. Please try again.")
  }

  def updateItem(partValue: String, partName: String, item: InventoryItem) = {
    var badCount = false

    partName match {
      case "itemNumber" =>
        InventoryChangeAudit.newChangeAudit(
          inventoryItem = item,
          originalItemNumber = item.itemNumber.get,
          newItemNumber = partValue
        )

        item.itemNumber(partValue).saveMe
      case "description" =>
        InventoryChangeAudit.newChangeAudit(
          inventoryItem = item,
          originalDescription = item.description.get,
          newDescription = partValue
        )

        item.description(partValue).saveMe
      case "count" =>
        val realCount = tryo(partValue.toInt)

        if (realCount.isEmpty)
          badCount = true
        else {
          InventoryService.updateItemCount(item, item.total.get, realCount.openOr(0))
        }
      case _ => Full(item)
    }

    if (badCount) {
      Alert("Count is not valid. Data not saved")
    }
    else
      Noop
  }

  def adjustCount(
    adjustmentType: String,
    item: InventoryItem,
    detailsRenderer: IdMemoizeTransform
  )() = {
    val count = item.total.get

    adjustmentType match {
      case "subtract" =>
        InventoryService.updateItemCount(item, count, count - 1)
      case "add" =>
        InventoryService.updateItemCount(item, count, count + 1)
      case _ =>
    }

    detailsRenderer.setHtml
  }

  def unitOfMeasureDropdown = {
    val possibleUnits = List(UnitOfMeasure.Each, UnitOfMeasure.Carton)

    SHtml.selectObj(
      possibleUnits.map(unitOfMeasure => (unitOfMeasure, unitOfMeasure.toString)),
      chosenUnit,
      (possibleUnit: UnitOfMeasure.Value) => chosenUnit = Full(possibleUnit)
    )
  }

  def inventoryItemDetailBindings(detailsRenderer: IdMemoizeTransform, item: InventoryItem) = {
    val itemNumber = item.itemNumber.get
    val description = item.description.get
    val count = item.total.toString

    ".change-item-number" #> ajaxText(itemNumber, possibleItem => updateItem(possibleItem, "itemNumber", item)) &
      ".change-description" #> ajaxText(description, possibleDescription => updateItem(possibleDescription, "description", item)) &
      ".change-count" #> ajaxText(count, possibleCount => updateItem(possibleCount, "count", item))
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".inventory-items [class+]" #> "current" &
    ".create" #> idMemoize { renderer =>
      "#new-item-number" #> ajaxText(newItemNumber, newItemNumber = _) &
      "#new-description" #> ajaxText(newDescription, newDescription = _) &
      "#new-count" #> ajaxText(newCount, newCount = _) &
      "#new-unit-of-measure-select" #> unitOfMeasureDropdown &
      "#create-item" #> SHtml.ajaxSubmit("Create Item", () => createItem)
    } &
    "tbody" #> inventoryItems.sortWith(_.itemNumber.get < _.itemNumber.get).map { item =>
      idMemoize { detailsRenderer =>
        ".item-entry" #> {
          ".item-number *" #> item.itemNumber.get &
          ".description *" #> item.description.get &
          ".unit-of-measure *" #> item.unitOfMeasure.get.toString &
          ".count *" #> item.total.get &
          ".subtract-one [onclick]" #> ajaxInvoke(adjustCount("subtract", item, detailsRenderer) _) &
          ".add-one [onclick]" #> ajaxInvoke(adjustCount("add", item, detailsRenderer) _) &
          ".expand-row [onclick]" #> ajaxInvoke(() => {
            if (currentItem.isEmpty) {
              currentItem = Full(item)
            } else {
              currentItem = Empty
            }

            detailsRenderer.setHtml
          })
        } &
        ".info [class+]" #> {if (currentItem.isEmpty) "" else "expanded"} &
        "^ [class+]" #> {if (currentItem.isEmpty) "" else "expanded"} &
        ".item-info" #> {
          if (!currentItem.isEmpty) {
            inventoryItemDetailBindings(detailsRenderer, item)
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
