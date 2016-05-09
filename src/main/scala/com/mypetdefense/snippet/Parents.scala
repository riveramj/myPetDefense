package com.mypetdefense.snippet

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

  val menu = Menu.i("Parents") / "parents"
}

class Parents extends Loggable {

  val parents = User.findAll(By(User.userType, UserType.Parent))

  def getNextShipDate(parent: User) = {
    Subscription.find(By(Subscription.user, parent)).map(_.nextShipDate.get)
  }

  def render = {
    ".parent" #> parents.map { parent =>
      val dateFormat = new SimpleDateFormat("MMM dd")

      ".name *" #> parent.name &
      ".email *" #> parent.email &
      ".phone *" #> parent.phone &
      ".referer *" #> parent.retailor.obj.map(_.name.get) &
      ".ship-date *" #> getNextShipDate(parent).map(dateFormat.format(_))
    }
  }
}

