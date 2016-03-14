package com.fleaTick.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._

import com.fleaTick.snippet.PetChoice._
import com.fleaTick.snippet.PetSize._
import com.fleaTick.snippet.PetProduct._
import com.fleaTick.snippet.Plan._
import com.fleaTick.model._

object Checkout extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Checkout") / "checkout"
}

class Checkout extends Loggable {

  var selectedProduct = ""

  def render = {
    "#type span *" #> petChoice.is.map(_.toString) &
    "#size span *" #> petSize.is.map(_.toString) &
    "#product span *" #> petProduct.is &
    "#plan span *" #> plan.is
  }
}



