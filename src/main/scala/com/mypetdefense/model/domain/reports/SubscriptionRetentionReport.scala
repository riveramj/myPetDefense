package com.mypetdefense.model.domain.reports

import java.time.LocalDate
import java.time.temporal.ChronoUnit.MONTHS
import java.util.Date

import com.mypetdefense.typeclasses.ToCsvStringConverter
import com.mypetdefense.util.DateHelper._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._

final case class SubscriptionRetentionReport(retentions: List[SubscriptionRetentionForPeriod]) {
  def toCsv: String = {
    val periodsCount = retentions.headOption.map(_.shipmentCounts.length).getOrElse(12)
    s"""Cohort,${(1 to periodsCount).mkString(",")}
       |${retentions.map(_.toCsvRow(periodsCount)).mkString("\n")}
       |""".stripMargin
  }

  def toJson: JArray = JArray(retentions.map(_.toJson))
}

final case class SubscriptionRetentionForPeriod(
    period: RetentionPeriod,
    shipmentCounts: List[Int]
) {
  def shipmentPercentages: List[Int] =
    shipmentCounts.headOption.fold(List.empty[Int]) { scale =>
      shipmentCounts.map(c => (c * 100.0 / scale).ceil.toInt)
    }

  def toCsvRow(periodsCount: Int) =
    s"$period,${shipmentPercentages.mkString(",")}${"," * (periodsCount - shipmentCounts.length)}"

  def toJson: JObject =
    (("period"                 -> period.toJson)
      ~ ("shipmentCounts"      -> shipmentCounts)
      ~ ("shipmentPercentages" -> shipmentPercentages))
}

final case class RetentionPeriod(month: Int, year: Int) {
  def +(periods: Int): RetentionPeriod = toDesiredPeriod(_ plusMonths periods)
  def -(periods: Int): RetentionPeriod = toDesiredPeriod(_ minusMonths periods)

  def -(period: RetentionPeriod): Int = {
    val startOfThisMonth = LocalDate.of(year, month, 1)
    val startOfThatMonth = LocalDate.of(period.year, period.month, 1)
    MONTHS.between(startOfThatMonth, startOfThisMonth).toInt
  }

  def next: RetentionPeriod = this + 1
  def prev: RetentionPeriod = this - 1

  def startDate: Date = makeDate(year, month, 1)

  override def toString = f"$month%02d-$year"

  def toJson: JString = JString(toString)

  private def toDesiredPeriod(toDesiredDate: LocalDate => LocalDate): RetentionPeriod = {
    val startOfThisMonth    = LocalDate.of(year, month, 1)
    val startOfDesiredMonth = toDesiredDate(startOfThisMonth)
    RetentionPeriod(startOfDesiredMonth.getMonthValue, startOfDesiredMonth.getYear)
  }
}

object SubscriptionRetentionReport {
  implicit val toCsvStringConverter: ToCsvStringConverter[SubscriptionRetentionReport] =
    ToCsvStringConverter.fromFunction(_.toCsv)
}

object RetentionPeriod {
  def current(): RetentionPeriod =
    fromLocalDate(LocalDate.now())

  def fromDate(date: Date): RetentionPeriod =
    fromLocalDate(LocalDate.from(date.toInstant.atZone(zoneId)))

  def fromLocalDate(date: LocalDate): RetentionPeriod =
    RetentionPeriod(date.getMonthValue, date.getYear)
}
