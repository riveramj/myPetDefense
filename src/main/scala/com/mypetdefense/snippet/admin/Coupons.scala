package com.mypetdefense.snippet
package admin

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._

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

  def render = {
    ".coupon" #> coupons.map { coupon =>
      ".code *" #> coupon.couponCode &
      ".months *" #> coupon.freeMonths &
      ".agent *" #> coupon.referer.obj.map(_.name.get)
    }
  }
}
