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

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._

object Coupons extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Coupons") / "admin" / "coupons" >>
    adminUser >>
    loggedIn
}

class Coupons extends Loggable {
  val coupons = Coupon.findAll()
  val allAgents = Agent.findAll()

  var codeName = ""
  var freeMonths = ""
  var agency: Box[Agent] = Empty

  def agentDropdown = {
    SHtml.selectObj(
        allAgents.map(agent => (agent, agent.name.get)),
        Empty,
        (agent: Agent) => agency = Full(agent)
      )
  }

  def createCoupon = {
    val validateFields = List(
      checkEmpty(codeName, "#code-name"),
      checkNumber(freeMonths.trim(), "#free-months")
    ).flatten

    if(validateFields.isEmpty) {
      Coupon.createCoupon(
        codeName.toLowerCase().trim(),
        tryo(freeMonths.trim().toInt).openOr(0),
        agency
      )
      S.redirectTo(Coupons.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".coupons [class+]" #> "current" &
    "#code-name" #> text(codeName, codeName = _) &
    "#free-months" #> text(freeMonths, freeMonths = _) &
    "#agent-container #agent-select" #> agentDropdown &
    "#create-item" #> SHtml.ajaxSubmit("Create Coupon", () => createCoupon) &
    ".coupon" #> coupons.map { coupon =>
      ".code *" #> coupon.couponCode &
      ".months *" #> coupon.freeMonths &
      ".agent *" #> coupon.referer.obj.map(_.name.get)
    }
  }
}
