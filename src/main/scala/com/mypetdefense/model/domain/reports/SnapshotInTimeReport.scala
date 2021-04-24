package com.mypetdefense.model.domain.reports

import com.mypetdefense.typeclasses.ToCsvStringConverter

case class SnapshotInTimeReport(
                                 activeSubscriptions: Long,
                                 activePets: Long,
                                 statsByProgram: Iterable[StatsByProgram],
                                 statsByAgency: Iterable[StatsByAgency],
                                 petsByProgramByAgency: Iterable[StatsByProgramByAgency]
) {
  def toCsv: String =
    s"""Active Subscriptions,$activeSubscriptions
       |Active Pets,$activePets
       |,
       |Stats by Program,
       |Program,Subscriptions,Pets
       |${statsByProgram.toList.sortBy(_.subscriptionCount).map(_.toCsvRow).mkString("\n")}
       |,
       |Stats by Agency,
       |Agency,Subscriptions,Pets
       |${statsByAgency.toList.sortBy(_.subscriptionCount).map(_.toCsvRow).mkString("\n")}
       |,
       |Stats by Program by Agency,
       |Agency,Subscriptions,Pets
       |${petsByProgramByAgency.toList.sortBy(_.agencyName).map(_.toCsvRow).mkString("\n")}
       |""".stripMargin
}

case class AllActivePetsSnapshotReport(activePets: Long) {
  def toCsv: String =
    s"Active Pets,$activePets"
}
case class StatsByProgram(programName: String, subscriptionCount: Int, petCount: Int) {
  def toCsvRow = s"$programName,$subscriptionCount,$petCount"
}

case class StatsByAgency(agencyName: String, subscriptionCount: Int, petCount: Int) {
  def toCsvRow = s"$agencyName,$subscriptionCount,$petCount"
}

case class StatsByProgramByAgency(agencyName: String, petsByProgramForAgency: Iterable[StatsByProgram]) {
  def toCsvRow = {
    s"""$agencyName,
       |${petsByProgramForAgency.toList.sortBy(_.subscriptionCount).map(_.toCsvRow).mkString("\n")}
       |""".stripMargin
  }
}

object SnapshotInTimeReport {
  implicit val toCsvStringConverter: ToCsvStringConverter[SnapshotInTimeReport] =
    ToCsvStringConverter.fromFunction(_.toCsv)
}
