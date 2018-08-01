package com.mypetdefense.snippet 
package friendsfamily 

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmd._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._
import net.liftweb.common._
import net.liftweb.http._
  import js.JsCmds._
import net.liftweb.mapper.{By, NullRef}

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.Paths._
import com.mypetdefense.util._
import com.mypetdefense.actor._
import com.mypetdefense.service._

object Dashboard extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Friends Family Dashboard") / "friendsfamily" / "dashboard" >>
    adminUser >>
    loggedIn

  val friendsFamilyLabelsExportMenu = Menu.i("Export Labels") / "friendsfamily" / "dashboard" / "export_friendsfamily_labels.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(exportFriendsFamilyUspsLabels _)

  def exportFriendsFamilyUspsLabels: Box[LiftResponse] = {
    LabelExportService.exportFriendsFamilyUspsLabels()
  }
}

class Dashboard extends Loggable {
  
  val newOrders = FriendsFamilyOrder.findAll(NullRef(FriendsFamilyOrder.shippedDate))

  def orderHasShipped_?(order: FriendsFamilyOrder) = {
    !tryo(order.shippedDate.get.toString).isEmpty
  }

  def shipOrder(order: FriendsFamilyOrder, renderer: IdMemoizeTransform)() = {
    order.shippedDate(new Date()).saveMe
    renderer.setHtml
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".dashboard [class+]" #> "current" &
    ".shipment" #> newOrders.map { order =>
      val products = order.products
      val nameAddress =
        s"""${order.name}
        |${order.street1}
        |${order.street2}
        |${order.city}, ${order.state} ${order.zip}""".stripMargin.replaceAll("\n\n", "\n")
      
      ".name-address *" #> nameAddress &
      ".product" #> products.map { product =>
        ".count *" #> product.quantity.get &
        ".name *" #> product.product.obj.map(_.description.get)
      } &
      ".ship-it" #> SHtml.idMemoize { shipButtonRenderer =>
        if (orderHasShipped_?(order)) {
          ".ship [class+]" #> "shipped" &
          ".ship *" #> "Shipped" &
          ".ship [disabled]" #> "disabled"
        } else {
          ".ship [onclick]" #> SHtml.ajaxInvoke(shipOrder(order, shipButtonRenderer) _)
        }
      }
    }
  }
}

