package com.mypetdefense.util

import com.mypetdefense.model.Shipment
import net.liftweb.util.Helpers.tryo

object CalculationHelper {

  def getShipmentAmountPaid(shipment: Shipment): BigDecimal = {
    val amountPaid = tryo(BigDecimal(shipment.amountPaid.get)).getOrElse(BigDecimal(0d))
    val taxesPaid  = tryo(BigDecimal(shipment.taxPaid.get)).getOrElse(BigDecimal(0d))
    amountPaid - taxesPaid
  }

  def calculateOccurrences[K, I](input: List[I], toKeyFun: I => K): Map[K, Int] = {
    input
      .foldLeft(Map.empty[K, Int]) { (map, inputElement) =>
        val key   = toKeyFun(inputElement)
        val count = map.get(key).fold(1)(_ + 1)
        map.updated(key, count)
      }
  }

}
