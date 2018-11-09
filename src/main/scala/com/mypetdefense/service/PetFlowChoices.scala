package com.mypetdefense.service

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._

import com.mypetdefense.model._
import com.mypetdefense.snippet._

import scala.collection.mutable.LinkedHashMap

object PetFlowChoices extends Loggable {
  object freeMonths extends SessionVar[Box[Int]](Empty)
  object discount extends SessionVar[Box[Double]](Empty)
  object subtotal extends SessionVar[Box[Double]](Empty)
  object priceCode extends SessionVar[Box[String]](Empty)
  object coupon extends SessionVar[Box[Coupon]](Empty)
  object total extends SessionVar[Box[Double]](Empty)
  object recentProduct extends RequestVar[Box[Product]](Empty)
  object shoppingCart extends SessionVar[Map[Long, (String, Product, Double)]](Map())
  object purchased extends SessionVar[Box[Boolean]](Empty)
  object boxSalesInfo extends SessionVar[Box[(Int, Int, Double)]](Empty)
}

object BoxDetailsFlow extends Loggable {
  object shoppingCart extends SessionVar[List[(Int, PetBox)]](Nil)
}
