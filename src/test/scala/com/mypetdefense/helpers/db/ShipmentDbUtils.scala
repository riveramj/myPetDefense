package com.mypetdefense.helpers.db

import java.util.Date

import com.mypetdefense.helpers.Random.{generateMoneyString, generateString}
import com.mypetdefense.model.{Insert, Shipment, ShipmentStatus, Subscription, User}
import net.liftweb.common.{Box, Full}

object ShipmentDbUtils {

  def createShipment(
      user: User,
      subscription: Subscription,
      stripePaymentId: String = generateString,
      stripeChargeId: Box[String] = Full(generateString),
      amountPaid: String = generateMoneyString,
      taxPaid: String = generateMoneyString,
      inserts: List[Insert] = List.empty,
      shipmentStatus: ShipmentStatus.Value = ShipmentStatus.Delivered,
      sendFreeUpgrade: Boolean = false
  ): Shipment =
    Shipment.createShipment(
      user,
      subscription,
      stripePaymentId,
      stripeChargeId,
      amountPaid,
      taxPaid,
      inserts,
      shipmentStatus,
      sendFreeUpgrade
    )

  def createShipmentWithDates(
      user: User,
      subscription: Subscription,
      dateProcessed: Date,
      expectedShipDate: Date,
      dateShipped: Date,
      dateReceived: Date
  ): Shipment =
    createShipment(user, subscription)
      .dateProcessed(dateProcessed)
      .expectedShipDate(expectedShipDate)
      .dateShipped(dateShipped)
      .dateReceived(dateReceived)
      .saveMe()

}
