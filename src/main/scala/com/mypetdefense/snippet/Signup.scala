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

object Signup extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.param[User](
    "Signup", "Signup",
    accessKey => AccessKeyService.findUserByKey(accessKey),
    user => user.accessKey.get
  ) / "signup" 

}

class Signup extends Loggable {
  import Signup._

  val newUser = Signup.menu.currentValue

  println(newUser)

  def render = {
    "#foo" #> ""
  }
}

