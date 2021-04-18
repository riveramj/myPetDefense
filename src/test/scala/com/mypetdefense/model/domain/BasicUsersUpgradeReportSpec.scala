package com.mypetdefense.model.domain

import com.mypetdefense.model.domain.reports._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class BasicUsersUpgradeReportSpec extends AnyFlatSpec with Matchers {
  it should "properly converted to csv when needed" in {
    val upgradesByAgency = List(
      UpgradesByAgency("TPP",180, 5.4, 90, 94),
      UpgradesByAgency("My Pet Defense", 15, 3.2, 10, 12)
    )

    val report = BasicUsersUpgradeReport(
      70,
      30,
      upgradesByAgency
    )
    val reportCsv = report.toCsv

    val expectedReportString =
      s"""Total Upgrades,100
         |,
         |Active Upgrades,70
         |Inactive Upgrades,30
         |,
         |Upgrades by Agency
         |Agency,Upgraded Subscriptions,Average Shipment Before Upgrade, Active Upgrades, Active Pets
         |My Pet Defense,15,3.2,10,12
         |TPP,180,5.4,90,94
         |""".stripMargin
    reportCsv shouldBe expectedReportString
  }
}