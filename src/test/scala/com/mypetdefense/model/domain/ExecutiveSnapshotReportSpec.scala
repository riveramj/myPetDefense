package com.mypetdefense.model.domain

import com.mypetdefense.model.domain.reports._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ExecutiveSnapshotReportSpec extends AnyFlatSpec with Matchers {
  it should "properly converted to csv when needed" in {
    val allAccountsReport           = AllAccountsReport(1, 10)
    val upgradedSubscriptionsReport = UpgradedSubscriptionsReport(30, 12, 11)
    val activeUpgradedPetsBySize    = List(PetsBySize("big", 13), PetsBySize("xxs", 10))
    val cancelledUpgradedPetsBySize = List(PetsBySize("strange", 4), PetsBySize("nano", 3))
    val cancelledUpgradedSubsByPetCount = List(
      CancelledUpgradedSubscriptionsByCount(1, 10),
      CancelledUpgradedSubscriptionsByCount(132, 55)
    )
    val cancelledUpgradedSubsByShipmentCount = List(
      CancelledUpgradedSubscriptionsByCount(63, 32),
      CancelledUpgradedSubscriptionsByCount(2, 5)
    )
    val activeUpgradesByAgency = List(CountedByAgency("mpd", 32), CountedByAgency("tpp", 999))
    val cancelledUpgradesByAgency =
      List(CountedByAgency("someAgency", 32), CountedByAgency("notSome", 33))

    val report = ExecutiveSnapshotReport(
      allAccountsReport,
      upgradedSubscriptionsReport,
      activeUpgradedPetsBySize,
      cancelledUpgradedPetsBySize,
      cancelledUpgradedSubsByPetCount,
      cancelledUpgradedSubsByShipmentCount,
      activeUpgradesByAgency,
      cancelledUpgradesByAgency
    )
    val reportCsv = report.toCsv

    val expectedReportString =
      s"""All Accounts,
         |Active Subscriptions ,1
         |Active Pets,10
         |,
         |Upgraded Subscription,
         |Active Subscriptions ,30
         |Active Pets,12
         |Cancelled Subscriptions,11
         |,
         |Active Upgraded Pets By Product,
         |big,13
         |xxs,10
         |,
         |Cancelled Upgraded Pets By Product,
         |nano,3
         |strange,4
         |,
         |Cancelled Pet Count By Subscription,
         |1 pets,10
         |132 pets,55
         |,
         |Cancelled Subscription Count By Shipment Count,
         |2 shipments,5
         |63 shipments,32
         |,
         |Active Upgrades By Agency,
         |mpd,32
         |tpp,999
         |,
         |,
         |Cancelled Upgrades By Agency,
         |someAgency,32
         |notSome,33
         |""".stripMargin
    reportCsv shouldBe expectedReportString
  }
}
