package com.mypetdefense.service

import com.mypetdefense.model.{Status, User}
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.util.SecurityContext
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._

object LoginService extends Loggable {
  def userCanLogIn_?(user: User, password: String, facebookId: String): Box[Boolean] = {
    if (!facebookId.isEmpty)
      Full(true)
    else if (!User.isCorrectPassword_?(password, user)) {
      Failure(S.?("login-invalidEmailOrPassword"))
    } else if (user.status.get == Status.Inactive) {
      Failure("Inactive user")
    } else {
      Full(true)
    }
  }

  def login(
      email: String,
      password: String,
      facebookId: String,
      boxLogin: Boolean = false
  ): JsCmd = {
    val validateFields =
      if (facebookId.isEmpty)
        List(
          checkEmpty(email, "#email"),
          checkEmpty(email, "#password")
        ).flatten
      else
        Nil

    if (validateFields.isEmpty) {
      User.findByEmailOrId(email, facebookId) match {
        case Full(user) =>
          userCanLogIn_?(user, password, facebookId) match {
            case Failure(msg, _, _) =>
              logger.info("\nOn login, we got %s" format msg)

              (ValidationError("#email", "Invalid") &
                ValidationError("#password", "Invalid"))

            case Empty =>
              logger.error("Got an unexpected password check")

              (ValidationError("#email", "Invalid") &
                ValidationError("#password", "Invalid"))

            case _ =>
              if (boxLogin) {
                SecurityContext.logIn(user)
                Noop
              } else {
                SecurityContext.loginRedirectUser(user)
              }
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
}
