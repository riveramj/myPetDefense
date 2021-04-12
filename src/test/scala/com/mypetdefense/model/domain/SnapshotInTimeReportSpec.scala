package com.mypetdefense.model.domain

import com.mypetdefense.model.domain.reports._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class SnapshotInTimeReportSpec extends AnyFlatSpec with Matchers {
  it should "properly converted to csv when needed" in {
    val statsByProgram = List(StatsByProgram("Flea & Tick Only",28, 32), StatsByProgram("Health and Wellness", 35, 42))
    val statsByAgency = List(StatsByAgency("TPP",11, 15), StatsByAgency("My Pet Defense", 21, 30))
    val statsByProgramByAgency = List(
      StatsByProgramByAgency("TPP", List(StatsByProgram("Flea & Tick Only", 25, 32), StatsByProgram("Health and Wellness", 41, 42))),
      StatsByProgramByAgency("My Pet Defense", List(StatsByProgram("Flea & Tick Only", 8, 11), StatsByProgram("Health and Wellness", 13, 19))),
    )

    val report = SnapshotInTimeReport(
      150,
      100,
      statsByProgram,
      statsByAgency,
      statsByProgramByAgency
    )
    val reportCsv = report.toCsv

    val expectedReportString =
      s"""Active Subscriptions,150
         |Active Pets,100
         |,
         |Stats by Program,
         |Program,Subscriptions,Pets
         |Flea & Tick Only,28,32
         |Health and Wellness,35,42
         |,
         |Stats by Agency,
         |Agency,Subscriptions,Pets
         |TPP,11,15
         |My Pet Defense,21,30
         |,
         |Stats by Program by Agency,
         |Agency,Subscriptions,Pets
         |My Pet Defense,
         |Flea & Tick Only,8,11
         |Health and Wellness,13,19
         |
         |TPP,
         |Flea & Tick Only,25,32
         |Health and Wellness,41,42
         |
         |""".stripMargin
    reportCsv shouldBe expectedReportString
  }
}