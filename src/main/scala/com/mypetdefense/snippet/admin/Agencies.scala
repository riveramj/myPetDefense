package com.mypetdefense.snippet
package admin

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
  import js.JsCmds._
import net.liftweb.mapper.By

import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._

object Agencies extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

    val menu = Menu.i("Agencies") / "admin" / "agencies" 
}

class Agencies extends Loggable {
  var name = ""

  val agencies = Agency.findAll()

  def createAgency = {
    val validateFields = List(
      checkEmpty(name, "#name")
    ).flatten

    if(validateFields.isEmpty) {
      Agency.createNewAgency(
        name.trim()
      )
      S.redirectTo(Agencies.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".agencies [class+]" #> "current" &
    "#name" #> text(name, name = _) &
    "#create-item" #> SHtml.ajaxSubmit("Create Agency", () => createAgency) &
    ".agency" #> agencies.map { agency =>
      ".name *" #> agency.name &
      ".customer-count *" #> agency.customers.size &
      ".coupon-count *" #> agency.coupons.size
    }
  }
}



