package com.mypetdefense.snippet.login

import com.mypetdefense._
import com.mypetdefense.model.User
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import com.mypetdefense.util.SecurityContext
import net.liftweb._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._
import net.liftweb.util.Helpers._

object Signup extends Loggable {
  import net.liftweb.sitemap._
  import Loc._

  val menu = 
    Menu.param[User](
      "Signup", "Signup",
      accessKey => KeyService.findUserByKey(accessKey, "accessKey"),
      user => user.accessKey.get
    ) / "signup" >>
    MatchWithoutCurrentValue >>
    IfValue(_.isDefined, ()=> {
      RedirectResponse(Login.menu.loc.calcDefaultHref)
    })

}

class Signup extends Loggable {

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
        KeyService.removeKey(user, "accessKey")
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
