package com.mypetdefense.service

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._

import com.mypetdefense.model._
import com.mypetdefense.snippet._

import scala.collection.mutable.LinkedHashMap

object PetFlowChoices extends Loggable {

  object petId extends SessionVar[Box[Long]](Empty)
  object petChoice extends SessionVar[Box[AnimalType.Value]](Empty)
  object petSize extends SessionVar[Box[AnimalSize.Value]](Empty)
  object petProduct extends SessionVar[Box[Product]](Empty)
  object total extends SessionVar[Box[Double]](Empty)
  object freeMonths extends SessionVar[Box[Int]](Empty)

  object completedPets extends SessionVar[LinkedHashMap[Long, Pet]](LinkedHashMap.empty)
}
