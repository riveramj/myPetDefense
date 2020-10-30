package com.mypetdefense.model.domain.reports

import com.mypetdefense.typeclasses.ToCsvStringConverter

case class ExecutiveSnapshotReport(
    allAccountsReport: AllAccountsReport,
    upgradedSubscriptionsReport: UpgradedSubscriptionsReport,
    activeUpgradedPetsByProduct: Iterable[PetsByProduct],
    cancelledUpgradedPetsByProduct: Iterable[PetsByProduct],
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
       |${activeUpgradedPetsByProduct.toList.sortBy(_.product).map(_.toCsvRow).mkString("\n")}
       |,
       |Cancelled Upgraded Pets By Product,
       |${cancelledUpgradedPetsByProduct.toList.sortBy(_.product).map(_.toCsvRow).mkString("\n")}
       |,
       |Cancelled Pet Count By Subscription,
       |${cancelledUpgradedSubsByPetCount.toList
         .sortBy(_.count)
         .map(_.toCsvRow("pets"))
         .mkString("\n")}
       |,
       |Cancelled Subscription Count By Shipment Count,
       |${cancelledUpgradedSubsByShipmentCount.toList
         .sortBy(_.count)
         .map(_.toCsvRow("shipments"))
         .mkString("\n")}
       |,
       |Active Upgrades By Agency,
       |${activeUpgradesByAgency.toList.sortBy(_.count).map(_.toCsvRow).mkString("\n")}
       |,
       |,
       |Cancelled Upgrades By Agency,
       |${cancelledUpgradesByAgency.toList.sortBy(_.count).map(_.toCsvRow).mkString("\n")}
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

case class PetsByProduct(product: String, count: Int) {
  def toCsvRow: String = s"$product,$count"
}

case class CancelledUpgradedSubscriptionsByCount(count: Int, subsCount: Int) {
  def toCsvRow(prefix: String): String = s"$count $prefix,$subsCount"
}

case class CountedByAgency(agencyName: String, count: Int) {
  def toCsvRow = s"$agencyName,$count"
}

object ExecutiveSnapshotReport {
  implicit val toCsvStringConverter: ToCsvStringConverter[ExecutiveSnapshotReport] =
    (input: ExecutiveSnapshotReport) => input.toCsv
}
