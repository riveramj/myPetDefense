package com.fleaTick.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._

import com.fleaTick.snippet.PetChoice._

object PetSize extends Loggable {
  import net.liftweb.sitemap._
    import Loc._

  val menu = Menu.i("Pet Size") / "pet-size"

  object petSize extends SessionVar[Box[String]](Empty)
}

class PetSize extends Loggable {
  import PetSize._

  var selectedSize = ""

  val petSizes = 
    if (petChoice.is == Full("Dog"))
      SHtml.radio(Seq("Small", "Medium", "Large", "X-Large"), petSize.is, selectedSize = _).toForm
    else
      SHtml.radio(Seq("Small", "Large"), petSize.is, selectedSize = _).toForm

  def render = {
    def chooseSize() = {
      petSize(Full(selectedSize))
    }

    ClearClearable andThen
    ".pet-size" #> petSizes.map { pet =>
      "input" #> pet
    } &
    "button" #> SHtml.onSubmitUnit(chooseSize)
  }
}

