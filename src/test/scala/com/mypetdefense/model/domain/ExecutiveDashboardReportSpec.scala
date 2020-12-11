package com.mypetdefense.model.domain

import com.mypetdefense.model.domain.reports._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ExecutiveDashboardReportSpec extends AnyFlatSpec with Matchers {
  "TodayRelatedData" should "properly calculate things" in {
    val data = TodayRelatedData(2, 3, 10)

    val expectedMonthDiff       = -1
    val expectedLastYearDiff    = -8
    val expectedPercentageMonth = -33
    val expectedPercentageYear  = -80

    data.monthDiff shouldBe expectedMonthDiff
    data.yearDiff shouldBe expectedLastYearDiff
    data.monthDiffPercentage shouldBe expectedPercentageMonth
    data.yearDiffPercentage shouldBe expectedPercentageYear
  }
  "MTDData" should "properly calculate things" in {
    val data = MTDData(16, 8, 3)

    val expectedMonthDiff       = 8
    val expectedLastYearDiff    = 13
    val expectedPercentageMonth = 100
    val expectedPercentageYear  = 433

    data.monthDiff shouldBe expectedMonthDiff
    data.yearDiff shouldBe expectedLastYearDiff
    data.monthDiffPercentage shouldBe expectedPercentageMonth
    data.yearDiffPercentage shouldBe expectedPercentageYear
  }
  "YTDData" should "properly calculate things" in {
    val data = YTDData(100, 32, 500)

    val expectedMonthDiff       = 68
    val expectedLastYearDiff    = -400
    val expectedPercentageMonth = 212
    val expectedPercentageYear  = -80

    data.monthDiff shouldBe expectedMonthDiff
    data.yearDiff shouldBe expectedLastYearDiff
    data.monthDiffPercentage shouldBe expectedPercentageMonth
    data.yearDiffPercentage shouldBe expectedPercentageYear
  }

  "ExecutiveDashboardReport" should "properly calculate newStartsPercentage" in {
    val todayData          = TodayRelatedData(2, 3, 10)
    val mtdData            = MTDData(16, 8, 3)
    val ytdData            = YTDData(100, 32, 500)
    val mtdShipmentData    = MTDShipmentsData(0, 0)
    val todayShipmentsData = TodayShipmentsData(0, 0)

    val report = ExecutiveDashboardReport(
      todayData,
      mtdData,
      ytdData,
      mtdShipmentData,
      todayShipmentsData,
      0,
      0,
      0
    )
    report.newStartsPercentage shouldBe Array(85, 14, 2)
  }
}
