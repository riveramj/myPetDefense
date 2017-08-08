package com.mypetdefense.service

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._

import com.mypetdefense.model._
import com.mypetdefense.snippet._

import scala.collection.mutable.LinkedHashMap

object PetFlowChoices extends Loggable {
  object freeMonths extends SessionVar[Box[Int]](Empty)
  object coupon extends SessionVar[Box[Coupon]](Empty)
  object total extends SessionVar[Box[Double]](Empty)

  object completedPets extends SessionVar[LinkedHashMap[Long, Pet]](LinkedHashMap.empty)
  
  object shoppingCart extends SessionVar[Seq[(Long, String, Product, Double)]](Seq.empty)
}
