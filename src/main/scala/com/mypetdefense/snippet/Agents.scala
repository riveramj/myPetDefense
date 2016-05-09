package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
import net.liftweb.mapper.By

import com.mypetdefense.model._

object Agents extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Agents") / "agents"
}

class Agents extends Loggable {

  val agent = User.findAll(By(User.userType, UserType.Agent))

  def render = {
    ".agent" #> agent.map { agent =>

      ".name *" #> agent.name &
      ".email *" #> agent.email &
      ".phone *" #> agent.phone 
    }
  }
}



