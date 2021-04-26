package com.mypetdefense.model.domain

import com.mypetdefense.model.domain.reports._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class CustomerLifespanReportSpec extends AnyFlatSpec with Matchers {
  it should "properly converted to csv when needed" in {
    val lifespansByAgency = List(
      LifespanByAgency(
        "TPP",
        "Active",
        LifespanStatistics(1004,904,804,704,604,504),
        LifespanStatistics(1005,905,805,705,605,505)
      ),
      LifespanByAgency(
        "TPP",
        "Inactive",
        LifespanStatistics(1006,906,806,706,606,506),
        LifespanStatistics(1007,907,807,707,607,507)
      ),
      LifespanByAgency(
        "My Pet Defense",
        "Active",
        LifespanStatistics(1008,908,808,708,608,508),
        LifespanStatistics(1009,909,809,709,609,509)
      ),
      LifespanByAgency(
        "My Pet Defense",
        "Inactive",
        LifespanStatistics(1010,910,810,710,610,510),
        LifespanStatistics(1011,911,811,711,611,511)
      )
    )

    val report = CustomerLifespanReport(lifespansByAgency)
    val reportCsv = report.toCsv

    val expectedReportString =
      s""",,0-4 months,4-6 months,6-12 months,1-2 years,2-3 years,3+ years
         |My Pet Defense Active,Subscriptions,1008,908,808,708,608,508
         |,Pets,1009,909,809,709,609,509
         |
         |My Pet Defense Inactive,Subscriptions,1010,910,810,710,610,510
         |,Pets,1011,911,811,711,611,511
         |
         |TPP Active,Subscriptions,1004,904,804,704,604,504
         |,Pets,1005,905,805,705,605,505
         |
         |TPP Inactive,Subscriptions,1006,906,806,706,606,506
         |,Pets,1007,907,807,707,607,507
         |
         |""".stripMargin
    reportCsv shouldBe expectedReportString
  }
}