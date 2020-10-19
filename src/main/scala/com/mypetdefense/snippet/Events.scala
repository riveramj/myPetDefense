package com.mypetdefense.snippet

import scala.xml._

import net.liftweb._
import common._
import http._
import LiftRules._
import js._
import JsCmds._
import JE._
import JsExp._
import util._
import Helpers._
import json._
import JsonDSL._
import Extraction._

class MyPetDefenseEvent(eventName: String) extends JsCmd {
  import Serialization._

  implicit def typeHints: AnyRef with Formats = Serialization.formats(NoTypeHints)

  def toJsCmd: String = {
    Call("myPetDefenseSite.event", eventName, decompose(this)).cmd.toJsCmd
  }
}
