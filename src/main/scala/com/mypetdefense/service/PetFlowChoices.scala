package com.mypetdefense.service

import com.mypetdefense.model._
import net.liftweb.common._
import net.liftweb.http._

import scala.collection.mutable.LinkedHashMap

object PetFlowChoices extends Loggable {
  object freeMonths    extends SessionVar[Box[Int]](Empty)
  object discount      extends SessionVar[Box[Double]](Empty)
  object subtotal      extends SessionVar[Box[Double]](Empty)
  object priceCode     extends SessionVar[Box[String]](Empty)
  object coupon        extends SessionVar[Box[Coupon]](Empty)
  object monthlyTotal  extends SessionVar[Box[BigDecimal]](Empty)
  object todayTotal    extends SessionVar[Box[BigDecimal]](Empty)
  object recentProduct extends RequestVar[Box[FleaTick]](Empty)
  object shoppingCart  extends SessionVar[Map[Long, (String, FleaTick, Double)]](Map())
  object purchased     extends SessionVar[Box[Boolean]](Empty)

  object petChoice     extends SessionVar[Box[AnimalType.Value]](Empty)
  object petBoxType    extends SessionVar[Box[BoxType.Value]](Empty)
  object petId         extends SessionVar[Box[Long]](Empty)
  object completedPets extends SessionVar[LinkedHashMap[Long, PendingPet]](LinkedHashMap.empty)
  object petCount      extends SessionVar[Box[Int]](Empty)

  object woofTraxOfferCode  extends SessionVar[Box[String]](Empty)
  object woofTraxUserId     extends SessionVar[Box[String]](Empty)
}

object TreatsFlow extends Loggable {
  object treatShoppingCart extends SessionVar[Map[Product, Int]](Map())
  object treatSale         extends SessionVar[Box[(Double, Map[Product, Int])]](Empty)
}

object AddOnFlow extends Loggable {
  object addOnShoppingCart extends SessionVar[Map[Product, Int]](Map())
  object addOnSale         extends SessionVar[Map[Product, Int]](Map())
}
