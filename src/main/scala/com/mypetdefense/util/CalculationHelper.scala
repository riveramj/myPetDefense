package com.mypetdefense.util

import com.mypetdefense.model.Shipment
import net.liftweb.util.Helpers.tryo

object CalculationHelper {

  def getShipmentAmountPaid(shipment: Shipment): BigDecimal = {
    val amountPaid = tryo(BigDecimal(shipment.amountPaid.get)).getOrElse(BigDecimal(0d))
    val taxesPaid  = tryo(BigDecimal(shipment.taxPaid.get)).getOrElse(BigDecimal(0d))
    amountPaid - taxesPaid
  }

}
