package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
import net.liftweb.mapper.By

import com.mypetdefense.model._

object Leads extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Leads") / "leads"
}

class Leads extends Loggable {

  val leads = Lead.findAll()

  def render = {
    ".parent" #> leads.map { lead =>

      ".name *" #> lead.name &
      ".email *" #> lead.email &
      ".phone *" #> lead.phone &
      ".referer *" #> lead.referer.obj.map(_.name.get)
    }
  }
}


