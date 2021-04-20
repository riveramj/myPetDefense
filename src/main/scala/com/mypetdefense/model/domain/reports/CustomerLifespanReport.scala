package com.mypetdefense.model.domain.reports

import com.mypetdefense.typeclasses.ToCsvStringConverter

case class CustomerLifespanReport(
  lifespansByAgency: List[LifespanByAgency]
) {
  def toCsv: String = {
    s""",,0-4 months,4-6 months,6-12 months,1-2 years,2-3 years,3+ years
       |${lifespansByAgency.sortBy(x => s"${x.agencyName} ${x.status}").map(_.toCsvRow).mkString("\n")}
       |""".stripMargin
  }
}

case class LifespanByAgency(
  agencyName: String,
  status: String,
  subscriptionStatistics: LifespanStatistics,
  petStatistics: LifespanStatistics
) {
  def toCsvRow =
    s"""$agencyName $status,Subscriptions,${subscriptionStatistics.toCsvRow}
       |,Pets,${petStatistics.toCsvRow}
       |""".stripMargin
}
case class LifespanStatistics(
  zeroFourMonths: Int,
  fourSixMonths: Int,
  sixTwelveMonths: Int,
  oneTwoYears: Int,
  twoThreeYears: Int,
  threePlusYears: Int
) {
  def toCsvRow =
    s"$zeroFourMonths,$fourSixMonths,$sixTwelveMonths,$oneTwoYears,$twoThreeYears,$threePlusYears"
}

sealed trait LifeSpan extends Product with Serializable
case object ZeroFourMonths extends LifeSpan
case object FourSixMonths extends LifeSpan
case object SixTwelveMonths extends LifeSpan
case object OneTwoYears extends LifeSpan
case object TwoThreeYears extends LifeSpan
case object ThreePlusYears extends LifeSpan

object CustomerLifespanReport {
  implicit val toCsvStringConverter: ToCsvStringConverter[CustomerLifespanReport] =
    ToCsvStringConverter.fromFunction(_.toCsv)
}
