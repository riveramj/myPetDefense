package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
import net.liftweb.mapper.{BySql, IHaveValidatedThisSQL}

import java.time.MonthDay
import java.time.format.DateTimeFormatter

import com.mypetdefense.model._

object Dashboard extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Dashboard") / "dashboard"
}

class Dashboard extends Loggable {
  val currentMonthDay = MonthDay.now().getDayOfMonth()
  val upcomingSubscriptions = Subscription.findAll(
    BySql("shipDay between ? and ?", 
      IHaveValidatedThisSQL("mike","2016-04-16"),
      currentMonthDay, 
      currentMonthDay + 3
    )
  )

  def render = {
    ".shipment" #> upcomingSubscriptions.map { subscription =>
      val productNames = subscription.getProducts.groupBy(_.name)
      val currentMonth = MonthDay.now().getMonthValue()
      val shipDate = MonthDay.of(1, 31).withMonth(currentMonth)

      ".ship-on *" #> shipDate.format(DateTimeFormatter.ofPattern("MMM dd")) &
      ".name *" #> subscription.parent.obj.map(_.name) &
      ".products" #> productNames.map { case (name, product) =>
        ".amount *" #> product.size &
        ".product-name *" #> name
      }
    }
  }
}
