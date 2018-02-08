package com.mypetdefense.snippet 

import net.liftweb._
  import http.SHtml._
  import util._
  import util.Helpers._
  import common._
  import http._
  import js._
  import JsCmds._

import scala.collection.mutable.ListBuffer
import net.liftweb.mapper.By
import java.nio.file.{Paths, Files}

import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.service._
  import ValidationService._
  import PetFlowChoices._

object ValentinePicture extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.param[String]("Your Valentine Picture", "Your Valentine Picture",
      Full(_),
      string => string
    ) / "valentine" / * >>
    TemplateBox(() => Templates("valentine-picture" :: Nil))
}

class ValentinePicture extends Loggable {
  import PetFlowChoices._

  val possibleCoupon = Coupon.find(By(Coupon.couponCode, "cold5k"))
  PetFlowChoices.coupon(possibleCoupon)
  PetFlowChoices.priceCode(Full("cold5k"))

  def render = {
    val dogName = ValentinePicture.menu.loc.currentValue.map(_.toString).openOr("").toUpperCase

    var photoNumber = 1
    var photoNames = new ListBuffer[String]()

    while (
      Files.exists(Paths.get(s"src/main/webapp/images/cold5k/${dogName}-${photoNumber}.jpg")) == true
    ) {
      photoNames += s"${dogName}-${photoNumber}.jpg"
      photoNumber += 1
    }

    ".photos" #> photoNames.map { photo =>
      ".photo img [src]" #> s"/images/cold5k/${photo}"
    }
  }
}
