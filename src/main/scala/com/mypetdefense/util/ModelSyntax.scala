package com.mypetdefense.util

import com.mypetdefense.model.{Subscription, User}

object ModelSyntax {
  implicit class ListOfUsersSyntax(val v: List[User]) extends AnyVal {
    def getSubscriptions: List[Subscription] = v.flatMap(_.subscription.toList)
  }
}
