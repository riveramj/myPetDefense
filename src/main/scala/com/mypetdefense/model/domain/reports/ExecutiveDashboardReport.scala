package com.mypetdefense.model.domain.reports

import com.mypetdefense.util.CalculationHelper

case class ExecutiveDashboardReport(
    newStartsTodayData: TodayRelatedData,
    newStartsMTDData: MTDData,
    newStartsYTDData: YTDData,
    mtdShipmentData: MTDShipmentsData,
    todayShipmentsData: TodayShipmentsData,
    remainingMonthSubscriptions: Int,
    newUserCount: Int,
    cancellationsCount: Int
) {
  val totalStarts: Int =
    newStartsTodayData.newStartsToday + newStartsMTDData.newStartsMTD + newStartsYTDData.newStartsYTD
  val newStartsPercentage: Array[Int] = Array(
    calcStartPercentage(newStartsYTDData.newStartsYTD),
    calcStartPercentage(newStartsMTDData.newStartsMTD),
    calcStartPercentage(newStartsTodayData.newStartsToday)
  )

  private def calcStartPercentage(starts: Int): Int =
    ((starts / totalStarts.toDouble) * 100).round.toInt
}

case class TodayRelatedData(
    newStartsToday: Int,
    newStartsTodayLastMonth: Int,
    newStartsTodayLastYear: Int
) {
  val monthDiff: Int = newStartsToday - newStartsTodayLastMonth
  val yearDiff: Int  = newStartsToday - newStartsTodayLastYear

  def monthDiffPercentage: Int =
    CalculationHelper.calculatePercentageDiff(newStartsTodayLastMonth, newStartsToday).toInt
  def yearDiffPercentage: Int =
    CalculationHelper.calculatePercentageDiff(newStartsTodayLastYear, newStartsToday).toInt
}

case class MTDData(
    newStartsMTD: Int,
    newStartsMTDLastMonth: Int,
    newStartsMTDLastYear: Int
) {
  val monthDiff: Int = newStartsMTD - newStartsMTDLastMonth
  val yearDiff: Int  = newStartsMTD - newStartsMTDLastYear

  def monthDiffPercentage: Int =
    CalculationHelper.calculatePercentageDiff(newStartsMTDLastMonth, newStartsMTD).toInt
  def yearDiffPercentage: Int =
    CalculationHelper.calculatePercentageDiff(newStartsMTDLastYear, newStartsMTD).toInt
}

case class YTDData(
    newStartsYTD: Int,
    newStartsYTDLastMonth: Int,
    newStartsYTDLastYear: Int
) {
  val monthDiff: Int = newStartsYTD - newStartsYTDLastMonth
  val yearDiff: Int  = newStartsYTD - newStartsYTDLastYear

  def monthDiffPercentage: Int =
    CalculationHelper.calculatePercentageDiff(newStartsYTDLastMonth, newStartsYTD).toInt
  def yearDiffPercentage: Int =
    CalculationHelper.calculatePercentageDiff(newStartsYTDLastYear, newStartsYTD).toInt
}

case class MTDShipmentsData(
    numberOfShipments: Int,
    totalAmount: BigDecimal
)

case class TodayShipmentsData(
    numberOfShipments: Int,
    totalAmount: BigDecimal
)
