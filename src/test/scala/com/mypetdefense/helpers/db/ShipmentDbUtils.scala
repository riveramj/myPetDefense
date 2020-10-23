package com.mypetdefense.helpers.db

import com.mypetdefense.generator.ShipmentCreateGeneratedData
import com.mypetdefense.model.{Insert, Shipment, Subscription, User}

object ShipmentDbUtils {
  def createShipment(
      user: User,
      sub: Subscription,
      gData: ShipmentCreateGeneratedData,
      inserts: List[Insert] = List.empty
  ): Shipment = {
    Shipment.createShipment(
      user,
      sub,
      gData.stripePaymentId,
      gData.stripeChargeId,
      gData.amountPaid,
      gData.taxPaid,
      inserts,
      gData.shipmentStatus,
      gData.sendFreeUpgrade
    )
  }
}
