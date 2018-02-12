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

  val namePhotoCount = List(
      ("DALTON", 2),
      ("Bo", 3),
      ("Bacon", 6),
      ("Sookie", 2),
      ("Zoey", 2),
      ("Jerry", 2),
      ("Penelope", 1),
      ("Hope", 3),
      ("Cooper", 5),
      ("Marlowe", 1),
      ("Sadie", 1),
      ("Jack", 4),
      ("LUCYLU-SCARLETT", 2),
      ("Leber", 2),
      ("ROB-KATE", 2),
      ("zeke", 1),
      ("Loki", 1),
      ("Cairo", 1),
      ("Sam", 2),
      ("RUBIE-DIXIE-PIPER", 1),
      ("Sage", 3),
      ("Baxter", 2),
      ("Wilson", 1),
      ("Jax", 2),
      ("Cherry", 2),
      ("Cece", 2),
      ("CARBON-PIEDMONT", 1),
      ("Luna", 3),
      ("MAYA-SKY", 2),
      ("2Cooper", 2),
      ("Tyson", 2),
      ("Dixie", 1),
      ("Sullivan", 1),
      ("Beauregard", 2),
      ("Annie", 1),
      ("Rocky", 2),
      ("Fitz", 3),
      ("DIPPY-BAILEY", 3),
      ("Doc", 1),
      ("Gus", 1),
      ("Felon", 4),
      ("COPPER-DODGE", 2),
      ("Sammie", 3),
      ("Samson", 3),
      ("DARCY-BAILEY", 3),
      ("FARLEY-MATILDA", 1),
      ("DEXTER-BAILEY", 1),
      ("Willie", 2),
      ("Horace", 1),
      ("Lucy", 2)
    )

  def render = {
    val dogName = ValentinePicture.menu.loc.currentValue.map(_.toString).openOr("").toUpperCase

    var photoNumber = 0
    var photoNames = new ListBuffer[String]()
    val dogPhotoCount = (namePhotoCount.find { case (name, count) => name.toUpperCase == dogName}).map(_._2).getOrElse(0)

    while (photoNumber < dogPhotoCount) {
      photoNumber += 1
      photoNames += s"${dogName}-${photoNumber}.jpg"
    }

    ".photos [class+]" #> (if (photoNames.size < 3) "solo" else "") & 
    ".photos .photo" #> photoNames.map { photo =>
      "img [src]" #> s"/images/cold5k/${photo}"
    }
  }
}
