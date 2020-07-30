package com.mypetdefense.snippet.login

import com.mypetdefense._
import com.mypetdefense.model.User
import net.liftweb._
import net.liftweb.common._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._

object ResetPasswordSent extends Loggable {
  import net.liftweb.sitemap._
  import Loc._

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
