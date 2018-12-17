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

object Products extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Friends Family Products") / "friendsfamily" / "products" >>
    mpdAdmin >>
    loggedIn
}

class Products extends Loggable {
  
  val products = FriendsFamilyProduct.findAll()

  def render = {
    ".products [class+]" #> "current" &
    ".product" #> products.sortBy(_.sku.get).map { product =>
      ".sku *" #> product.sku.get &
      ".description *" #> product.description.get
    }
  }
}
