package com.mypetdefense.model.domain.reports

import com.mypetdefense.typeclasses.ToCsvStringConverter

case class QuickHitReport(
    allAccountsReport: AllAccountsReport,
    upgradedSubscriptionsReport: UpgradedSubscriptionsReport,
    activeUpgradedPetsBySize: Iterable[PetsBySize],
    canceledUpgradedPetsBySize: Iterable[PetsBySize],
    cancelledUpgradedSubsByPetCount: Iterable[CancelledUpgradedSubscriptionsByCount],
    cancelledUpgradedSubsByShipmentCount: Iterable[CancelledUpgradedSubscriptionsByCount],
    activeUpgradesByAgency: Iterable[CountedByAgency],
    cancelledUpgradesByAgency: Iterable[CountedByAgency]
) {
  def toCsv: String =
    s"""All Accounts,
       |${allAccountsReport.toCsv}
       |,
       |Upgraded Subscription,
       |${upgradedSubscriptionsReport.toCsv}
       |,
       |Active Upgraded Pets By Product,
       |${activeUpgradedPetsBySize.map(_.toCsvRow).mkString("\n")}
       |,
       |Cancelled Upgraded Pets By Product,
       |${canceledUpgradedPetsBySize.map(_.toCsvRow).mkString("\n")}
       |,
       |Cancelled Pet Count By Subscription,
       |${cancelledUpgradedSubsByPetCount.map(_.toCsvRow("pets").mkString("\n"))}
       |,
       |Cancelled Subscription Count By Shipment Count,
       |${cancelledUpgradedSubsByShipmentCount.map(_.toCsvRow("shipments").mkString("\n"))}
       |,
       |Active Upgrades By Agency,
       |${activeUpgradesByAgency.map(_.toCsvRow).mkString("\n")}
       |,
       |,
       |Cancelled Upgrades By Agency,
       |${cancelledUpgradesByAgency.map(_.toCsvRow).mkString("\n")}
       |""".stripMargin
}

case class AllAccountsReport(activeSubscriptions: Long, activePets: Long) {
  def toCsv: String =
    s"""Active Subscriptions ,$activeSubscriptions
       |Active Pets,$activePets""".stripMargin
}

case class UpgradedSubscriptionsReport(
    activeSubscriptions: Long,
    activePets: Long,
    cancelledSubscriptions: Long
) {
  def toCsv: String =
    s"""Active Subscriptions ,$activeSubscriptions
       |Active Pets,$activePets
       |Cancelled Subscriptions,$cancelledSubscriptions""".stripMargin
}

case class PetsBySize(size: String, count: Int) {
  def toCsvRow: String = s"$size,$count"
}

case class CancelledUpgradedSubscriptionsByCount(count: Int, subsCount: Int) {
  def toCsvRow(prefix: String): String = s"$count $prefix,$subsCount"
}

case class CountedByAgency(agencyName: String, count: Int) {
  def toCsvRow = s"$agencyName,$count"
}

object QuickHitReport {
  implicit val toCsvStringConverter: ToCsvStringConverter[QuickHitReport] =
    (input: QuickHitReport) => input.toCsv
}
