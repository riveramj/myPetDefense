package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb._
  import http.SHtml._
  import util._
  import util.Helpers._
  import common._
  import util.ClearClearable
  import http._
  import mapper.{By, NullRef}
  import js._
  import JsCmds._

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.Paths._
import com.mypetdefense.actor._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.util.SecurityContext._
import com.mypetdefense.service._
  import ValidationService._

object Profile extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Profile") / "profile" >>
    loggedIn >>
    parent
}

class Profile extends Loggable {
  val user = currentUser
  val stripeCustomerId = user.map(_.stripeId.get).openOr("")

  var email = user.map(_.email.get).openOr("")
  var oldPassword = ""
  var newPassword = ""

  def updateEmail() = {
    val validateFields = List(
        checkEmail(email, "#email")
      ).flatten

    if(validateFields.isEmpty) {
      for {
        user <- user
        } {
          user
            .email(email)
            .saveMe
        }
        S.redirectTo(Profile.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def updatePassword() = {
    val validateFields = List(
        checkEmpty(oldPassword, "#old-password"),
        checkEmpty(newPassword, "#new-password")
      ).flatten

    if(validateFields.isEmpty) {
      val passwordUpdated = (for {
        user <- user
      } yield {
        if (User.isCorrectPassword_?(oldPassword, user)) {
          User.setUserPassword(user, newPassword)
          true
        } else {
          false
        }
      }).openOr(false)

      if (passwordUpdated)
        S.redirectTo(Profile.menu.loc.calcDefaultHref)
      else
        ValidationError("#old-password", "Incorrect old password")

    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".profile a [class+]" #> "current" &
    "#user-email *" #> email &
    "#email" #> text(email, userEmail => email = userEmail.trim) &
    "#old-password" #> SHtml.password(oldPassword, oldPass => oldPassword = oldPass.trim) &
    "#new-password" #> SHtml.password(newPassword, newPass => newPassword = newPass.trim) &
    ".update-email" #> SHtml.ajaxSubmit("Save Changes", updateEmail) &
    ".update-password" #> SHtml.ajaxSubmit("Save Changes", updatePassword)
  }
}

