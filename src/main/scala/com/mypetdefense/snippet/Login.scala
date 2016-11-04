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

    S.request match {
      case Full(request) =>
        if (request.get_? && request.param("logout").isDefined) {
          logout()
        }
      case _ =>
    }

    def userCanLogIn_?(user: User, password: String): Box[Boolean] = {
      if (!User.isCorrectPassword_?(password, user)) {
        Failure(S.?("login-invalidEmailOrPassword"))
      } else {
        Full(true)
      }
    }
    
    def login() = {
      val validateFields = List(
        checkEmpty(email, "#email"),
        checkEmpty(email, "#password")
      ).flatten

      if(validateFields.isEmpty) {
        User.findByEmail(email) match {
          case Full(user) =>
            userCanLogIn_?(user, password) match {
              case Failure(msg, _, _) =>
                logger.info("\nOn login, we got %s" format msg)

                (ValidationError("#email", "Invalid") &
                  ValidationError("#password", "Invalid"))

              case Empty =>
                logger.error("Got an unexpected password check")
                
                (ValidationError("#email", "Invalid") &
                  ValidationError("#password", "Invalid"))

              case _ =>
               SecurityContext.loginRedirectUser(user)
            }

          case err =>
            logger.info("\nOn login, we got %s" format err)

            (ValidationError("#email", "Invalid") &
              ValidationError("#password", "Invalid"))
        }
      } else {
        validateFields.foldLeft(Noop)(_ & _)
      }
    }

    SHtml.makeFormsAjax andThen
    "#login-container" #> {
      "#email" #> SHtml.text(email, email = _) &
      "#password" #> SHtml.password(password, password = _) &
      "#login" #> SHtml.ajaxSubmit("Log In", () => login)
    }
  }
}
