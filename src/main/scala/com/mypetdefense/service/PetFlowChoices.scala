package com.mypetdefense.service

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._

import com.mypetdefense.model._
import com.mypetdefense.snippet._

object PetFlowChoices extends Loggable {

  object petChoice extends SessionVar[Box[AnimalType.Value]](Empty)
  object petSize extends SessionVar[Box[AnimalSize.Value]](Empty)
  object petProduct extends SessionVar[Box[Product]](Empty)
  object petName extends SessionVar[Box[String]](Empty)
  object birthday extends SessionVar[Box[java.util.Date]](Empty)
  object total extends SessionVar[Box[Double]](Empty)
}
