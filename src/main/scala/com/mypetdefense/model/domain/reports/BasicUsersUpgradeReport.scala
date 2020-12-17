package com.mypetdefense.model.domain.reports

import java.util.Date

final case class BasicUsersUpgradeReport(upgrades: List[BasicUserUpgradeReport])

final case class BasicUserUpgradeReport(
    subscriptionId: Long,
    shipmentCountAtUpgrade: Int,
    upgradeDate: Date
)
