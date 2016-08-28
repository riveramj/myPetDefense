package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._

import com.mypetdefense.service._
import com.mypetdefense.model.AnimalType

object Login extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Login") / "login"
}

class Login extends Loggable {
}
