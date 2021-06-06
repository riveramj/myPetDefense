package com.mypetdefense.util

import com.mypetdefense.model._
import net.liftweb.util.Helpers.tryo

import scala.collection.GenTraversableOnce

object CalculationHelper {

  def getShipmentAmountPaid(shipment: Shipment): BigDecimal = {
    val amountPaid = tryo(BigDecimal(shipment.amountPaid.get)).getOrElse(BigDecimal(0d))
    val taxesPaid  = tryo(BigDecimal(shipment.taxPaid.get)).getOrElse(BigDecimal(0d))
    amountPaid - taxesPaid
  }

  def countOccurrences[I](input: GenTraversableOnce[I]): Map[I, Int] =
    countOccurrencesByKey(input)(identity)

  def countOccurrencesByKey[K, I](input: GenTraversableOnce[I])(toKeyFun: I => K): Map[K, Int] = {
    input
      .foldLeft(Map.empty[K, Int]) { (map, inputElement) =>
        val key   = toKeyFun(inputElement)
        val count = map.get(key).fold(1)(_ + 1)
        map.updated(key, count)
      }
      .withDefaultValue(0)
  }
  /*
    used formula -> https://www.percentage-change-calculator.com/index.html
   */
  def calculatePercentageDiff(startValue: BigDecimal, endValue: BigDecimal): BigDecimal = {
    if (startValue.toInt == 0) 0
    else ((endValue - startValue) / startValue) * 100
  }

  def calculateInsertsWeight(
      fleaTick: Iterable[FleaTick],
      inserts: Iterable[Insert]
  ): BigDecimal = {
    val productWeight = fleaTick.map(fleaT => BigDecimal(fleaT.weight.get)).sum
    val insertWeight  = inserts.map(insert => BigDecimal(insert.weight.get)).sum

    val totalWeight = productWeight + insertWeight

    if (totalWeight < 4.0) BigDecimal(4.0) else totalWeight
  }

  def calculateOrderWeight(
                            subscriptionBoxes: List[SubscriptionBox],
                            shipment: Shipment
                          ): BigDecimal = {
    val groupedBoxes = subscriptionBoxes.groupBy(_.boxType.get)

    val boxWeights = groupedBoxes map {
      case (BoxType.basic, boxes) if !shipment.freeUpgradeSample.get => boxes.size * 5.0
      case (BoxType.basic, boxes) => boxes.size * 15.0
      case (BoxType.`complete`, boxes) => boxes.size * 15.0
      case (_,_) => 0.0
    }

    boxWeights.sum
  }
}
