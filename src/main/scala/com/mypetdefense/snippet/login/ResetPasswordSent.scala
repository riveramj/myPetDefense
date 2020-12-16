package com.mypetdefense.snippet.login

import com.mypetdefense.model.User
import net.liftweb.common._
import net.liftweb.mapper.By
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._

object ResetPasswordSent extends Loggable {
  import net.liftweb.sitemap._
  import Loc._

  val menu: Menu.ParamMenuable[User] = Menu.param[User](
    "Reset Password Sent",
    "Reset Password Sent",
    userId => User.find(By(User.userId, userId.toLong)),
    user => user.userId.get.toString
  ) / "reset-password-sent" >>
    Hidden
}

class ResetPasswordSent {
  val user: Box[User] = ResetPasswordSent.menu.currentValue
  def render: CssSel = {
    "#user-email *" #> user.map(_.email.get)
  }
}
