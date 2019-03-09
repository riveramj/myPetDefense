package com.mypetdefense.snippet

import net.liftweb._
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
  import snippet.admin.Dashboard
import com.mypetdefense.util.SecurityContext
import com.mypetdefense.actor._

object ResetPassword extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.param[User](
    "Reset Password", "Reset Password",
    accessKey => KeyService.findUserByKey(accessKey, "accessKey"),
    user => user.resetPasswordKey.get
  ) / "reset-password" >> 
    Hidden
}

class ResetPassword {
  val user = ResetPassword.menu.currentValue
  var password = ""
  def resetPassword() = user match {
    case Full(user) => 
      User.setUserPassword(user, password).resetPasswordKey("").saveMe
      EmailActor ! SendPasswordUpdatedEmail(user)

      SecurityContext.loginRedirectUser(user)
    case _ =>
      S.redirectTo(Login.menu.loc.calcDefaultHref)
  }
  def render = {
    SHtml.makeFormsAjax andThen
    "#password" #> SHtml.password("", password = _) &
    "type=submit" #> SHtml.ajaxSubmit("Reset Password", resetPassword _)
  }
}
