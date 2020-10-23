package com.mypetdefense.generator

import com.mypetdefense.model.{Insert, ShipmentStatus}
import net.liftweb.common.Box

case class ShipmentCreateGeneratedData(
    stripePaymentId: String,
    stripeChargeId: Box[String],
    amountPaid: String,
    taxPaid: String,
    shipmentStatus: ShipmentStatus.Value,
    sendFreeUpgrade: Boolean
)
