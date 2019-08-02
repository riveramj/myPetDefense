package com.mypetdefense.snippet

import net.liftweb._
  import mapper.By
  import util.Helpers._
  import http._
  import common._
  import sitemap.Menu
  import js._
      import JsCmds._

import com.mypetdefense.service._
    import ValidationService._

import com.mypetdefense._
  import model.{User, UserType}
  import snippet.admin.ShipmentDashboard
import com.mypetdefense.util.SecurityContext

object ResetPasswordSent extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.param[User](
    "Reset Password Sent", "Reset Password Sent",
    userId => User.find(By(User.userId, userId.toLong)),
    user => user.userId.get.toString
  ) / "reset-password-sent" >> 
    Hidden
}

class ResetPasswordSent {
  val user = ResetPasswordSent.menu.currentValue
  def render = {
    "#user-email *" #> user.map(_.email.get)
  }
}
