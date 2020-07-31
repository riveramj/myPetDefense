package com.mypetdefense.snippet.signup

import com.mypetdefense.model._
import net.liftweb.common._
import net.liftweb.http.{S, SHtml}
import net.liftweb.mapper.{By, NotBy}
import net.liftweb.util.Helpers._

object CatSignup extends Loggable {

  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu = Menu.i("Cat Coming Soon") / "cat"
}

class CatSignup extends Loggable {
  
  def render = {
    "#email" #> "foo"
  }
}
