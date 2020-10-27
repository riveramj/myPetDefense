package com.mypetdefense.model.domain.reports

case class QuickHitReport(
    allAccountsReport: AllAccountsReport,
    upgradedSubscriptionsReport: UpgradedSubscriptionsReport,
    activeUpgradedPetsBySize: Iterable[PetsBySize],
    canceledUpgradedPetsBySize: Iterable[PetsBySize],
    cancelledUpgradedSubsByPetCount: Iterable[CancelledUpgradedSubscriptionsByCount],
    cancelledUpgradedSubsByShipmentCount: Iterable[CancelledUpgradedSubscriptionsByCount],
    activeUpgradesByAgency: Iterable[CountedByAgency],
    cancelledUpgradesByAgency: Iterable[CountedByAgency]
)

case class AllAccountsReport(activeSubscriptions: Long, activePets: Long) {
  def toCsvRow: String =
    s"""Active Subscriptions ,$activeSubscriptions
       |Active Pets,$activePets""".stripMargin
}

case class UpgradedSubscriptionsReport(
    activeSubscriptions: Long,
    activePets: Long,
    cancelledSubscriptions: Long
) {
  def toCsvRow: String =
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
