package com.mypetdefense.snippet.login

import com.mypetdefense.actor._
import com.mypetdefense.model.User
import com.mypetdefense.service._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

object ForgotPassword extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Forgot Password") / "forgot-password" >>
    notLoggedIn
}

class ForgotPassword extends Loggable {
  var userEmail = ""
  def submitPasswordReset(): Unit = {
    User.findByEmail(userEmail) match {
      case Full(user) =>
        val userWithResetKey = KeyService.createResetKey(user)
        EmailActor ! SendPasswordResetEmail(userWithResetKey)
        S.redirectTo(ResetPasswordSent.menu.toLoc.calcHref(userWithResetKey))
      case _ =>
        logger.error(userEmail)
    }
  }
  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
      "#email" #> SHtml.text(userEmail, email => userEmail = email.trim) &
        "type=submit" #> SHtml.ajaxSubmit("Send Email", submitPasswordReset _)
  }
}
