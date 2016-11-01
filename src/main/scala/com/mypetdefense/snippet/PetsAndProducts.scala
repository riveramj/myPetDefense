package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.Paths._
import com.mypetdefense.actor._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.util.SecurityContext._
import com.mypetdefense.service.TaxJarService

object PetsAndProducts extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Pets and Products") / "pets-products" >>
    loggedIn >>
    parent
}

class PetsAndProducts extends Loggable {
}
