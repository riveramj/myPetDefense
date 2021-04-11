package com.mypetdefense.model.domain.reports

import com.mypetdefense.typeclasses.ToCsvStringConverter

case class SnapshotInTimeReport(
    activePets: Long,
    petsByProgram: Iterable[CountedByProgram],
    petsByAgency: Iterable[CountedByAgency],
    petsByProgramAgency: Iterable[CountedByProgramByAgency]
) {
  def toCsv: String =
    s"""Active Pets,$activePets
       |,
       |Pets by Program,
       ${petsByProgram.toList.sortBy(_.count).map(_.toCsvRow).mkString("\n")}
       |,
       |Pets by Agency,
       |${petsByAgency.toList.sortBy(_.count).map(_.toCsvRow).mkString("\n")}
       |,
       |Pets by Program by Agency,
       |${petsByProgramAgency.toList.sortBy(_.agencyName).map(_.toCsvRow).mkString("\n")}
       |""".stripMargin
}

case class AllActivePetsSnapshotReport(activePets: Long) {
  def toCsv: String =
    s"Active Pets,$activePets"
}
case class CountedByProgram(programName: String, count: Int) {
  def toCsvRow = s"$programName,$count"
}

case class CountedByProgramByAgency(agencyName: String, petsByProgramForAgency: Iterable[CountedByProgram]) {
  def toCsvRow = {
    s"""$agencyName,
       |${petsByProgramForAgency.toList.sortBy(_.count).map(_.toCsvRow).mkString("\n")}
       |""".stripMargin
  }
}

object SnapshotInTimeReport {
  implicit val toCsvStringConverter: ToCsvStringConverter[SnapshotInTimeReport] =
    ToCsvStringConverter.fromFunction(_.toCsv)
}
