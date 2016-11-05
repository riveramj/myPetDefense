package com.mypetdefense.snippet

import net.liftweb._
  import common._
  import util._
    import Helpers._
  import http._
    import SHtml._
  import sitemap._
  import sitemap.Loc._

import com.mypetdefense.service._
    import ValidationService._

import com.mypetdefense._
  import model.{User, UserType}
  import snippet.admin.Dashboard
import com.mypetdefense.util.SecurityContext
import com.mypetdefense.actor._

object ForgotPassword extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Forgot Password") / "forgot-password" >>
    notLoggedIn
}

class ForgotPassword extends Loggable {
  var userEmail = ""
  def submitPasswordReset() = {
    User.findByEmail(userEmail) match {
      case Full(user) => 
        val userWithResetKey = ResetKeyService.createResetKey(user)
        EmailActor ! SendPasswordResetEmail(userWithResetKey)
        S.redirectTo(ResetPasswordSent.menu.toLoc.calcHref(userWithResetKey))
      case _ => 
        println(userEmail)
    }
  }
  def render = {
    SHtml.makeFormsAjax andThen
    "#email" #> SHtml.text(userEmail, email => userEmail = email.trim) &
    "type=submit" #> SHtml.ajaxSubmit("Send Email", submitPasswordReset)
  }
}
