package com.mypetdefense.snippet

import com.mypetdefense.actor._
import com.mypetdefense.util.Paths
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

class Picture extends Loggable {
  var name      = ""
  var dogName   = ""
  var email     = ""
  var dogLove   = ""
  var instagram = ""

  def sendMessage(): Nothing = {
    EmailActor ! PictureEmail(
      name,
      email,
      dogName,
      instagram,
      dogLove
    )

    S.redirectTo(Paths.thanksPage.loc.calcDefaultHref)
  }

  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
      ".name" #> text(name, name = _) &
        ".dog-name" #> text(dogName, dogName = _) &
        ".email" #> text(email, email = _) &
        ".instagram" #> text(instagram, instagram = _) &
        ".dog-love" #> textarea(dogLove, dogLove = _) &
        "#send-message" #> ajaxSubmit("Submit", sendMessage _)
  }
}
