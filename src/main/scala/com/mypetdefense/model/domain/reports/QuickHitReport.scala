package com.mypetdefense.model.domain.reports

case class QuickHitReport(
    allAccountsReport: AllAccountsReport,
    upgradedSubscriptionsReport: UpgradedSubscriptionsReport,
    activeUpgradedPetsBySize: Iterable[PetsBySize],
    canceledUpgradedPetsBySize: Iterable[PetsBySize],
    cancelledUpgradedSubscriptionByPetCount: Iterable[CancelledUpgradedSubscriptionByCount],
    cancelledUpgradedSubscriptionByShipmentCount: Iterable[CancelledUpgradedSubscriptionByCount],
    activeUpgradesByAgency: Iterable[CancelledUpgradesByAgency],
    cancelledUpgradesByAgency: Iterable[CancelledUpgradesByAgency]
)

case class AllAccountsReport(activeSubscriptions: Long, activePets: Long)

case class UpgradedSubscriptionsReport(
    activeSubscriptions: Long,
    activePets: Long,
    cancelledSubscriptions: Long
)

case class PetsBySize(size: String, count: Int)

case class CancelledUpgradesByAgency(agencyName: String, count: Int)

case class CancelledUpgradedPetCountBySubscription(petsCount: Int, subsCount: Int)

case class CancelledUpgradedSubscriptionByCount(count: Int, subsCount: Int)
