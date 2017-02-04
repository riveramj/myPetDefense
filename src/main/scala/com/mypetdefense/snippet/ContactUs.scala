package com.mypetdefense.snippet 

import net.liftweb._
  import http.SHtml._
  import util._
  import util.Helpers._
  import common._
  import http._
  import js._
  import JsCmds._
  

import com.mypetdefense.service._
  import ValidationService._
import com.mypetdefense.actor._

case class HelpMessageSent() extends MyPetDefenseEvent("help-message-sent")

class ContactUs extends Loggable {
  var name = ""
  var email = ""
  var message = ""
  val sourcePage = S.uri
  
  def sendMessage() = {
    val validateFields = List(
      checkEmail(email, ".email"),
      checkEmpty(name, ".name"),
      checkEmpty(message, ".message")
    ).flatten

    if(validateFields.isEmpty) {
      EmailActor ! ContactUsEmail(name, email, message, sourcePage)
      
      HelpMessageSent()
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".name" #> text(name, name = _) &
    ".email" #> text(email, email = _) &
    ".message" #> textarea(message, message = _) &
    "#send-message" #> ajaxSubmit("Send", sendMessage)
  }
}

