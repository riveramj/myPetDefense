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

object Orders extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Friends Family Orders") / "friendsfamily" / "orders" >>
    adminUser >>
    loggedIn
}

class Orders extends Loggable {
  val orders = FriendsFamilyOrder.findAll()

  val dateFormat = new SimpleDateFormat("MMM dd, YYYY")

  def getShippedDate(order: FriendsFamilyOrder) = {
    val possibleDate = tryo(order.shippedDate.get)

    if (possibleDate != Full(null)) {
      possibleDate.map { date =>
        dateFormat.format(date).toString
      }.openOr("-")
    } else {
      "-"
    }
  }

  def render = {
    ".orders [class+]" #> "current" &
    ".shipment" #> orders.sortBy(_.createdAt.get.getTime).map { order =>
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
      ".shipped-date *" #> getShippedDate(order)
    }
  }
}

