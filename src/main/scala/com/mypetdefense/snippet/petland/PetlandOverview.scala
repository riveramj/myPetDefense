package com.mypetdefense.snippet
package petland

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
import com.mypetdefense.service.{CouponService, ReportingService}
import com.mypetdefense.util.{ClearNodesIf, SecurityContext}

object PetlandOverview extends Loggable { 
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Petland Overview") / "petland" / "store-overview" >>
    agencyUser >>
    petlandUser >>
    loggedIn
}

class PetlandOverview extends Loggable {
  val currentUser = SecurityContext.currentUser
  val agency = currentUser.flatMap(_.agency.obj)
  val agencyName = agency.map(_.name.get).openOr("")

  val users = User.findAll(
    By(User.userType, UserType.Parent)
  ).filter(_.referer.obj == agency)

  def render = {
    ".overview [class+]" #> "current" &
    ".user" #> users.map { user =>

      ".name *" #> user.name &
      ".status *" #> user.status.toString
    }
  }
}


