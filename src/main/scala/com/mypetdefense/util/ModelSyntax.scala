package com.mypetdefense.util

import com.mypetdefense.model.{Pet, Subscription, User}

object ModelSyntax {
  implicit class ListOfUsersSyntax(val v: List[User]) extends AnyVal {
    def getSubscriptions: List[Subscription] = v.flatMap(_.subscription.toList)
  }

  implicit class ListOfSubscriptionsSyntax(val v: List[Subscription]) extends AnyVal {
    def getAllActivePets: List[Pet] = v.flatMap(_.getPets)
  }

}
