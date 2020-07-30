package com.mypetdefense.snippet.login

import com.mypetdefense.service._
import net.liftweb._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util.Helpers._

object Login extends Loggable {
  import net.liftweb.sitemap._
  import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Login") / "login" >>
    notLoggedIn

  val logoutMenu = Menu.i("Logout") / "logout" >>
    EarlyResponse(logout _)

  def logout() = {
    S.session.foreach(_.destroySession())
    Full(RedirectResponse(menu.loc.calcDefaultHref))
  }
}

class Login extends Loggable {
  import Login._

  def render = {
    var email = ""
    var password = ""
    var fbId = ""

    S.request match {
      case Full(request) =>
        if (request.get_? && request.param("logout").isDefined) {
          logout()
        }
      case _ =>
    }

    SHtml.makeFormsAjax andThen
    "#login-container" #> {
      "#email" #> SHtml.text(email, email = _) &
      "#password" #> SHtml.password(password, password = _) &
      "#fb-id" #> SHtml.password(fbId, fbId = _) &
      "#login" #> SHtml.ajaxSubmit("Log In", () => LoginService.login(email, password, fbId))
    }
  }
}
