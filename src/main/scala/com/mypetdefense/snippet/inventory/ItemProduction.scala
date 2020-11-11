package com.mypetdefense.snippet
package inventory

import net.liftweb._
import sitemap.Menu
import http.SHtml._
import http._
import js.JsCmds._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.{ClearNodes, CssSel}
import net.liftweb.mapper.By
import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.service.InventoryService
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.actor._
import net.liftweb.http.js.JsCmd

import scala.xml.NodeSeq

object ItemProduction extends Loggable {
  import net.liftweb.sitemap._
  import Loc._
  import com.mypetdefense.util.Paths._

  val menu: Menu.Menuable = Menu.i("Item Production") / "inventory" / "item-production" >>
    mpdAdmin >>
    loggedIn
}

class ItemProduction extends Loggable {
  val inventoryItems: List[InventoryItem]   = InventoryItem.findAll()
  val inventoryRecipes: List[InventoryItem] = inventoryItems.filter(_.itemParts.toList != Nil)

  var buildNewItemRecipe: Box[InventoryItem]  = Empty
  var selectedItemPart: Box[InventoryItem]    = Empty
  var addItemPartToRecipe: Box[InventoryItem] = Empty
  var newItemRecipe: Box[InventoryItem]       = Empty
  var firstRecipeItem: Box[InventoryItem]     = Empty

  var currentItemRecipe: Box[InventoryItem] = Empty

  var buildRecipeItemCount = ""

  var newRecipePart: Box[InventoryItem] = Empty

  var reconciliationRenderer: Box[IdMemoizeTransform] = Empty
  var currentReconciliation: Box[ReconciliationEvent] = Empty

  def allRecipesDropDown: xml.Elem = {
    SHtml.ajaxSelectObj(
      List((Empty, "")) ::: inventoryRecipes.map(item => (Full(item), item.description.get)),
      Full(buildNewItemRecipe),
      (possibleItemRecipe: Box[InventoryItem]) => buildNewItemRecipe = possibleItemRecipe
    )
  }

  def allInventoryItemsDropDown: xml.Elem = {
    SHtml.ajaxSelectObj(
      List((Empty, "")) ::: inventoryItems.map(item => (Full(item), item.description.get)),
      Full(addItemPartToRecipe),
      (possibleItemPart: Box[InventoryItem]) => addItemPartToRecipe = possibleItemPart
    )
  }

  def newRecipeDropDown: xml.Elem = {
    SHtml.ajaxSelectObj(
      List((Empty, "")) ::: inventoryItems.map(item => (Full(item), item.description.get)),
      Full(addItemPartToRecipe),
      (possibleItem: Box[InventoryItem]) => newItemRecipe = possibleItem
    )
  }

  def firstRecipeItemDropDown: xml.Elem = {
    SHtml.ajaxSelectObj(
      List((Empty, "")) ::: inventoryItems.map(item => (Full(item), item.description.get)),
      Full(addItemPartToRecipe),
      (possibleItem: Box[InventoryItem]) => firstRecipeItem = possibleItem
    )
  }

  def startNewRecipe: JsCmd = {
    {
      for {
        finishedItem <- newItemRecipe
        itemPart     <- firstRecipeItem
      } yield {
        InventoryItemPart.createNewInventoryItemPart(finishedItem, itemPart)
        S.redirectTo(ItemProduction.menu.loc.calcDefaultHref)
      }
    }.openOr(Noop)
  }

  def createItemFromRecipe: JsCmd = {
    val validateFields = List(
      validNumber(buildRecipeItemCount, "#create-count")
    ).flatten

    if (validateFields.isEmpty) {
      val realCreateCount = tryo(buildRecipeItemCount.toInt).openOr(0)

      for {
        staleRecipe <- buildNewItemRecipe.toList
        recipe = staleRecipe.reload
        itemPart      <- recipe.itemParts.toList
        inventoryItem <- itemPart.itemPart.toList
      } yield {
        val currentCount = inventoryItem.total.get
        InventoryService.updateItemCount(
          inventoryItem,
          currentCount,
          currentCount - realCreateCount
        )
      }

      for {
        recipe <- buildNewItemRecipe
      } yield {
        val currentCount = recipe.total.get

        InventoryService.updateItemCount(recipe, currentCount, currentCount + realCreateCount)
      }

      S.redirectTo(ItemProduction.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def addItemToRecipe(
      finishedItem: InventoryItem,
      renderer: IdMemoizeTransform
  )(): JsCmd = {
    addItemPartToRecipe.map { itemPart =>
      InventoryItemPart.createNewInventoryItemPart(finishedItem, itemPart)

      selectedItemPart = Empty
      renderer.setHtml
    }.openOr(Noop)
  }

  def itemRecipeDetailBindings(
      detailsRenderer: IdMemoizeTransform,
      finishedItem: InventoryItem
  ): CssSel = {
    val itemParts = finishedItem.reload.itemParts.toList

    ".item-part" #> itemParts
      .sortWith(
        _.itemPart.obj
          .map(_.itemNumber.get)
          .openOr("") < _.itemPart.obj.map(_.itemNumber.get).openOr("")
      )
      .map { itemPart =>
        val item = itemPart.itemPart.obj

        ".part-item-number *" #> item.map(_.itemNumber.get) &
          ".part-item-description *" #> item.map(_.description.get)
      } &
      ".add-item-container" #> {
        ".part-item-select" #> allInventoryItemsDropDown &
          ".create-item .add-item-part [onclick]" #> SHtml.ajaxInvoke(
            addItemToRecipe(finishedItem, detailsRenderer) _
          )
      }
  }

  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
      ".item-production [class+]" #> "current" &
        ".create" #> idMemoize { renderer =>
          "#recipe-item-select" #> allRecipesDropDown &
            "#build-item-count" #> ajaxText(buildRecipeItemCount, buildRecipeItemCount = _) &
            "#create-item" #> SHtml.ajaxSubmit("Build Item", () => createItemFromRecipe)
        } &
        ".new-recipe" #> {
          "#new-recipe-select" #> newRecipeDropDown &
            "#new-recipe-item-select" #> firstRecipeItemDropDown &
            "#create-new-recipe" #> SHtml.ajaxSubmit("Start Recipe", () => startNewRecipe)
        } &
        "tbody" #> inventoryRecipes.sortWith(_.itemNumber.get < _.itemNumber.get).map { recipe =>
          idMemoize { detailsRenderer =>
            ".item-entry" #> {
              ".finished-item-number *" #> recipe.itemNumber.get &
                ".finished-item-description *" #> recipe.description.get &
                ".finished-item-count *" #> recipe.total.get &
                "^ [onclick]" #> ajaxInvoke(() => {
                  if (currentItemRecipe.isEmpty) {
                    currentItemRecipe = Full(recipe)
                  } else {
                    currentItemRecipe = Empty
                  }

                  detailsRenderer.setHtml
                })
            } &
              ".info [class+]" #> { if (currentItemRecipe.isEmpty) "" else "expanded" } &
              "^ [class+]" #> { if (currentItemRecipe.isEmpty) "" else "expanded" } &
              ".item-parts" #> {
                if (!currentItemRecipe.isEmpty) {
                  itemRecipeDetailBindings(detailsRenderer, recipe)
                } else {
                  "^" #> ClearNodes
                }
              }
          }
        }
  }
}
