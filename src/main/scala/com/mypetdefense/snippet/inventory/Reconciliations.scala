package com.mypetdefense.snippet
package inventory

import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.util.DateFormatters._
import com.mypetdefense.util.DateHelper._
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._
import net.liftweb.util.{ClearNodes, CssSel}

import scala.xml.NodeSeq

object Reconciliations extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Reconciliations") / "inventory" / "reconciliations" >>
    mpdAdmin >>
    loggedIn
}

class Reconciliations extends Loggable {
  val reconciliationDateFormat = `1/1/2021`

  val reconciliations: List[ReconciliationEvent] = ReconciliationEvent.findAll()

  var newDate                        = ""
  var chosenItem: Box[InventoryItem] = Empty

  var reconciliationRenderer: Box[IdMemoizeTransform] = Empty
  var currentReconciliation: Box[ReconciliationEvent] = Empty

  var actualItemCount = ""

  def createReconciliationEvent: JsCmd = {
    val validateFields = List(
      validDate(newDate, reconciliationDateFormat, "#new-date")
    ).flatten

    if (validateFields.isEmpty) {
      val realDate = reconciliationDateFormat._1.parse(newDate).toZonedDateTime

      ReconciliationEvent.createNewReconciliationEvent(realDate)

      S.redirectTo(Reconciliations.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def deleteReconciliation(reconciliation: ReconciliationEvent)(): Alert = {
    if (reconciliation.delete_!)
      S.redirectTo(Reconciliations.menu.loc.calcDefaultHref)
    else
      Alert("An error has occured. Please try again.")
  }

  def inventoryItemDropdown: xml.Elem = {
    val inventoryItems = InventoryItem.findAll()

    SHtml.ajaxSelectObj(
      List((Empty, "")) ::: inventoryItems.map(item => (Full(item), item.description.get)),
      Full(chosenItem),
      (possibleItem: Box[InventoryItem]) => chosenItem = possibleItem
    )
  }

  def addItemToReconciliation(
      reconciliationEvent: ReconciliationEvent,
      renderer: IdMemoizeTransform
  )(): JsCmd = {
    chosenItem.map { item =>
      val expectedCount = item.total.get
      val realCount     = tryo(actualItemCount.toInt).openOr(0)

      ItemReconciliation.createNewItemReconciliation(
        item,
        reconciliationEvent,
        realCount,
        expectedCount
      )

      chosenItem = Empty
      actualItemCount = ""

      renderer.setHtml
    }.openOr(Noop)
  }

  def reconciliationDetailBindings(
      detailsRenderer: IdMemoizeTransform,
      reconciliation: ReconciliationEvent
  ): CssSel = {
    val itemReconciliations =
      ItemReconciliation.findAll(By(ItemReconciliation.reconciliationEvent, reconciliation))

    ".item-reconciliation" #> itemReconciliations
      .sortWith(
        _.inventoryItem.obj
          .map(_.itemNumber.get)
          .openOr("") < _.inventoryItem.obj.map(_.itemNumber.get).openOr("")
      )
      .map { itemReconciliation =>
        val item = itemReconciliation.inventoryItem.obj

        ".item-number *" #> item.map(_.itemNumber.get) &
          ".description *" #> item.map(_.description.get) &
          ".expected-count *" #> itemReconciliation.expectedCount.get &
          ".actual-count *" #> itemReconciliation.actualCount.get
      } &
      ".add-item-container" #> {
        ".item-select" #> inventoryItemDropdown &
          ".actual-item-count" #> ajaxText(actualItemCount, actualItemCount = _) &
          ".create-item-container .create-item [onclick]" #> SHtml.ajaxInvoke(
            addItemToReconciliation(reconciliation, detailsRenderer) _
          )
      }
  }

  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
      ".reconciliation [class+]" #> "current" &
        ".create" #> idMemoize { renderer =>
          "#new-date" #> ajaxText(newDate, newDate = _) &
            "#create-item" #> SHtml.ajaxSubmit(
              "Create Reconciliation",
              () => createReconciliationEvent
            )
        } &
        "tbody" #> reconciliations
          .sortWith(_.eventDate.get.toInstant.toEpochMilli > _.eventDate.get.toInstant.toEpochMilli)
          .map { reconciliation =>
            idMemoize { detailsRenderer =>
              ".reconciliation-entry" #> {
                ".date *" #> reconciliation.eventDate.get.format(reconciliationDateFormat) &
                  "^ [onclick]" #> ajaxInvoke(() => {
                    if (currentReconciliation.isEmpty) {
                      currentReconciliation = Full(reconciliation)
                    } else {
                      currentReconciliation = Empty
                    }

                    detailsRenderer.setHtml
                  })
              } &
                ".info [class+]" #> { if (currentReconciliation.isEmpty) "" else "expanded" } &
                "^ [class+]" #> { if (currentReconciliation.isEmpty) "" else "expanded" } &
                ".reconciliation-info" #> {
                  if (!currentReconciliation.isEmpty) {
                    reconciliationDetailBindings(detailsRenderer, reconciliation)
                  } else {
                    "^" #> ClearNodes
                  }
                }
            }
          }
  }
}
