package com.mypetdefense.util

import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model.{Shipment, Subscription, User}
import com.mypetdefense.util.DateHelper.signupCancelDateFormat
import net.liftweb.common.Box
import net.liftweb.util.Helpers.tryo

object ModelSyntax {

  implicit class ShipmentSyntax(val v: Shipment) extends AnyVal {
    def getProcessDateOfShipment: LocalDate =
      v.dateProcessed.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate

    def getMailedDateOfShipment: Box[LocalDate] =
      tryo(v.dateShipped.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate)
  }

  implicit class UserSyntax(val v: User) extends AnyVal {
    def getCreatedDateOfUser: LocalDate =
      v.createdAt.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate
  }

  implicit class ListOfUsersSyntax(val v: List[User]) extends AnyVal {
    def getSubscriptions: List[Subscription] = v.flatMap(_.subscription.toList)
  }

  implicit class SubscriptionSyntax(val v: Subscription) extends AnyVal {
    def getStartDateOfSubscription: String =
      tryo(signupCancelDateFormat.format(v.startDate.get)).openOr("")

    def getCancelDateOfSubscription: String =
      tryo(signupCancelDateFormat.format(v.cancellationDate.get)).openOr("")

    def getCreatedDateOfSubscription: LocalDate =
      v.createdAt.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate

    def getCancelledDateOfSubscription: Box[LocalDate] =
      tryo(v.cancellationDate.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate)

    def getNextShipDate: LocalDate =
      v.nextShipDate.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate
  }

}
