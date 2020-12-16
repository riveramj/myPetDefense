package com.mypetdefense.snippet
package inventory

import com.mypetdefense.model._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

object InventoryChangeAudits extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Inventory Audit") / "inventory" / "audit" >>
    mpdAdmin >>
    loggedIn
}

class InventoryChangeAudits extends Loggable {
  val audits: List[InventoryChangeAudit] = InventoryChangeAudit.findAll()

  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
      ".inventory-audit [class+]" #> "current" &
        ".audit-entry" #> audits.sortWith(_.createdAt.get.getTime > _.createdAt.get.getTime).map {
          audit =>
            val oldCount = {
              if (audit.originalCount.get == -1)
                ""
              else
                audit.originalCount.get.toString
            }

            ".inventory-item *" #> audit.inventoryItem.obj.map(_.itemNumber.get) &
              ".old-number *" #> audit.originalItemNumber.get &
              ".new-number *" #> audit.newItemNumber.get &
              ".old-description *" #> audit.originalDescription.get &
              ".new-description *" #> audit.newDescription.get &
              ".old-uom *" #> audit.originalUnitOfMeasure.get &
              ".new-uom *" #> audit.newUnitOfMeasure.get &
              ".old-count *" #> oldCount &
              ".new-count *" #> audit.newCount.get
        }
  }
}
