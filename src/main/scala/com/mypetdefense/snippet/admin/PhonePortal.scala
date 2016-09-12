package com.mypetdefense.snippet
package admin

import net.liftweb._
  import sitemap.Menu
  import http.SHtml._
  import http._
  import js.JsCmds._

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.service.ValidationService._

object PhonePortal extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Phone Portal") / "admin" / "phone-portal"
}

class PhonePortal extends Loggable {

  def render = {
    SHtml.makeFormsAjax andThen
    ".phone-portal [class+]" #> "current" 
    //".create" #> {
      //".new-pet-name" #> ajaxText(petName, petName = _) &
      //".pet-type-select" #> petTypeRadio(renderer) &
      //".product-container .product-select" #> productDropdown &
      //".create-item-container .create-item" #> SHtml.ajaxSubmit("Add Pet", () => addPet(parent))
    //}
  }
}
