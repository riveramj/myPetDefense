package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
import net.liftweb.mapper.{BySql, IHaveValidatedThisSQL}

import java.time.MonthDay

import com.mypetdefense.model._

object Dashboard extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Dashboard") / "dashboard"
}

class Dashboard extends Loggable {
  val currentMonthDay = MonthDay.now().getDayOfMonth()
  val upcomingSubscriptions = Subscription.findAll(
    BySql("shipDate between ? and ?", 
      IHaveValidatedThisSQL("mike","2016-04-16"),
      currentMonthDay, 
      currentMonthDay + 3
    )
  )

  def render = {
    ".shipment" #> upcomingSubscriptions.map { subscription =>
      ".ship-on *" #> subscription.shipDate &
      ".name *" #> subscription.parent.obj.map(_.name)
    }
  }
}
