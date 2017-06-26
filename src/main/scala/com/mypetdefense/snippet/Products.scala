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

import com.mypetdefense.service._
  import ValidationService._
  import PetFlowChoices._

import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.model._
import com.mypetdefense.actor._

object Products extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Products") / "products"
}

class Products extends Loggable {
  def render = {
    "#foo" #> ""
  }
}
