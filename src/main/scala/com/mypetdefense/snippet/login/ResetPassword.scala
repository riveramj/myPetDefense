package com.mypetdefense.snippet.login

import com.mypetdefense.actor._
import com.mypetdefense.model.User
import com.mypetdefense.service._
import com.mypetdefense.util.SecurityContext
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

object ResetPassword extends Loggable {
  import net.liftweb.sitemap._
  import Loc._

  val menu: Menu.ParamMenuable[User] = Menu.param[User](
    "Reset Password",
    "Reset Password",
    accessKey => KeyService.findUserByKey(accessKey, "resetPasswordKey"),
    user => user.resetPasswordKey.get
  ) / "reset-password" >>
    Hidden
}

class ResetPassword {
  val user: Box[User] = ResetPassword.menu.currentValue
  var password        = ""
  def resetPassword(): Nothing = user match {
    case Full(user) =>
      User.setUserPassword(user, password).resetPasswordKey("").saveMe
      EmailActor ! SendPasswordUpdatedEmail(user)

      SecurityContext.loginRedirectUser(user)
    case _ =>
      S.redirectTo(Login.menu.loc.calcDefaultHref)
  }
  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
      "#password" #> SHtml.password("", password = _) &
        "type=submit" #> SHtml.ajaxSubmit("Reset Password", resetPassword _)
  }
}
