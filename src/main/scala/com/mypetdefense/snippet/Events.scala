package com.mypetdefense.snippet

import net.liftweb.http.SHtml.ajaxCall
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmds.Function
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

class CallableFunction(name:String, callback:(String)=>JsCmd, args:List[String] = List()) extends JsCmd {
  override val toJsCmd =
    Function(
      name, args,
      ajaxCall(JsRaw("Array.prototype.slice.call(arguments).join('|')"), callback)._2.cmd
    ).toJsCmd
}
