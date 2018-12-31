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
  import snippet.agency.AgencyOverview
import com.mypetdefense.util.SecurityContext

object Signup extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = 
    Menu.param[User](
      "Signup", "Signup",
      accessKey => AccessKeyService.findUserByKey(accessKey),
      user => user.accessKey.get
    ) / "signup" >>
    MatchWithoutCurrentValue >>
    IfValue(_.isDefined, ()=> {
      RedirectResponse(Login.menu.loc.calcDefaultHref)
    })

}

class Signup extends Loggable {
  import Signup._

  val newUser = Signup.menu.currentValue
  val email = newUser.map(_.email.get).openOr("")
  
  var password = ""
  var firstName = newUser.map(_.firstName.get).openOr("")
  var lastName = newUser.map(_.lastName.get).openOr("")

  def signup() = {
    val validateFields = List(
      checkEmpty(firstName, "#first-name"),
      checkEmpty(lastName, "#last-name"),
      checkEmpty(password, "#password")
    ).flatten

    if (validateFields.isEmpty) {
      newUser.map { user =>
        AccessKeyService.removeAccessKey(user)
        User.updatePendingUser(user, firstName, lastName, password)
        SecurityContext.loginRedirectUser(user)
      }.openOr(Noop)

    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }


  def render = {
    SHtml.makeFormsAjax andThen
    "#signup-container" #> {
      "#first-name" #> SHtml.text(firstName, firstName = _) &
      "#last-name" #> SHtml.text(lastName, lastName = _) &
      "#email [value]" #> email &
      "#password" #> SHtml.password(password, password = _) &
      "#signup" #> SHtml.ajaxSubmit("Sign Up", () => signup)
    }
  }
}

