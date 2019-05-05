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
  object recentProduct extends RequestVar[Box[FleaTick]](Empty)
  object shoppingCart extends SessionVar[Map[Long, (String, FleaTick, Double)]](Map())
  object purchased extends SessionVar[Box[Boolean]](Empty)
}

object TreatsFlow extends Loggable {
  object treatShoppingCart extends SessionVar[Map[Product, Int]](Map())
  object treatSale extends SessionVar[Box[(Double, Map[Product, Int])]](Empty)
}
