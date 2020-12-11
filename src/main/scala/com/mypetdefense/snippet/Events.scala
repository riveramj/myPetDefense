package com.mypetdefense.snippet

import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsExp._
import net.liftweb.http.js._
import net.liftweb.json.Extraction._
import net.liftweb.json._

class MyPetDefenseEvent(eventName: String) extends JsCmd {

  implicit def typeHints: AnyRef with Formats = Serialization.formats(NoTypeHints)

  def toJsCmd: String = {
    Call("myPetDefenseSite.event", eventName, decompose(this)).cmd.toJsCmd
  }
}
