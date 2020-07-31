package com.mypetdefense.snippet.signup

import com.mypetdefense.model.{User, UserType}
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.util.SecurityContext
import net.liftweb.common._
import net.liftweb.http.SHtml.text
import net.liftweb.http._
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.util.Helpers._

object CreateAccount extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu = Menu.i("Create Account") / "create-account" >> completedPet
}

class CreateAccount extends Loggable {
  var firstName = ""
  var lastName = ""
  var email = ""
  var password = ""
  var facebookId = ""

  def createAccount() = {
    val baseFields = List(
      checkEmail(email, "#email", true),
      checkEmpty(firstName, "#first-name"),
      checkEmpty(lastName, "#last-name")
    )

    val passwordError = checkEmpty(password, "#password")
    val facebookError = checkFacebookId(facebookId, "#facebook-id", true)

    val validateFields = {
      if (facebookId.isEmpty)
        passwordError :: baseFields
      else
        facebookError :: baseFields
    }.flatten

    if(validateFields.isEmpty) {
      val newUser = User.upsertUser(
        firstName,
        lastName,
        email,
        password,
        facebookId,
        UserType.Parent
      )

      SecurityContext.logIn(newUser)
      S.redirectTo(Checkout.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    "#first-name" #> text(firstName, userFirstName => firstName = userFirstName.trim) &
    "#last-name" #> text(lastName, userLastName => lastName = userLastName.trim) &
    "#email" #> text(email, userEmail => email = userEmail.trim) &
    "#password" #> SHtml.password(password, userPassword => password = userPassword.trim) &
    "#facebook-id" #> SHtml.password(facebookId, facebookId = _) &
    ".create-account" #> SHtml.ajaxSubmit("Continue", () => createAccount)
  }
}
