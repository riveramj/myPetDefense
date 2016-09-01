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

object Parents extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Parents") / "admin" / "parents" >>
    adminUser >>
    loggedIn
}

class Parents extends Loggable {
  val parents = User.findAll(By(User.userType, UserType.Parent))

  def render = {
    ".parents [class+]" #> "current" &
    ".parent" #> parents.map { parent =>
      val dateFormat = new SimpleDateFormat("MMM dd")

      val nextShipDate = Subscription.find(By(Subscription.user, parent)).map(_.nextShipDate.get)

      ".name *" #> parent.name &
      ".email *" #> parent.email &
      ".phone *" #> parent.phone &
      ".referer *" #> parent.referer.obj.map(_.name.get) &
      ".ship-date *" #> nextShipDate.map(dateFormat.format(_))
    }
  }
}

