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

object Users extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Users") / "admin" / "users" >>
    adminUser >>
    loggedIn
}

class Users extends Loggable {
  val users = User.findAll()

  def render = {
    ".user" #> users.map { user =>

      ".name *" #> user.name &
      ".email *" #> user.email &
      ".type *" #> user.userType
    }
  }
}


