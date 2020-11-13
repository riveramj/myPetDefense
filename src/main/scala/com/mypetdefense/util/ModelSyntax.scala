package com.mypetdefense.util

import com.mypetdefense.model._

object ModelSyntax {
  implicit class ListOfUsersSyntax(val v: List[User]) extends AnyVal {
    def getSubscriptions: List[Subscription] = v.flatMap(_.subscription.toList)
  }

  implicit class ListOfSubscriptionsSyntax(val v: List[Subscription]) extends AnyVal {
    def getAllActivePets: List[Pet] = v.flatMap(_.getPets)
  }

  implicit class ListOfShipmentsSyntax(val v: List[Shipment]) extends AnyVal {
    def sumAmountPaid: BigDecimal = v.map(_.amountPaid.get).map(amount => BigDecimal(amount)).sum
  }

}
