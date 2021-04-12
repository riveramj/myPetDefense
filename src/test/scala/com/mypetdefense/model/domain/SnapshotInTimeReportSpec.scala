package com.mypetdefense.model.domain

import com.mypetdefense.model.domain.reports._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class SnapshotInTimeReportSpec extends AnyFlatSpec with Matchers {
  it should "properly converted to csv when needed" in {
    val petsByProgram = List(CountedByProgram("Flea & Tick Only", 32), CountedByProgram("Health and Wellness", 42))
    val petsByAgency = List(CountedByAgency("TPP", 15), CountedByAgency("My Pet Defense", 30))
    val petsByProgramByAgency = List(
      CountedByProgramByAgency("TPP", List(CountedByProgram("Flea & Tick Only", 32), CountedByProgram("Health and Wellness", 42))),
      CountedByProgramByAgency("My Pet Defense", List(CountedByProgram("Flea & Tick Only", 11), CountedByProgram("Health and Wellness", 19))),
    )

    val report = SnapshotInTimeReport(
      100,
      petsByProgram,
      petsByAgency,
      petsByProgramByAgency
    )
    val reportCsv = report.toCsv

    val expectedReportString =
      s"""Active Pets,100
         |,
         |Pets by Program,
         |Flea & Tick Only,32
         |Health and Wellness,42
         |,
         |Pets by Agency,
         |TPP,15
         |My Pet Defense,30
         |,
         |Pets by Program by Agency,
         |My Pet Defense,
         |Flea & Tick Only,11
         |Health and Wellness,19
         |
         |TPP,
         |Flea & Tick Only,32
         |Health and Wellness,42
         |
         |""".stripMargin
    reportCsv shouldBe expectedReportString
  }
}