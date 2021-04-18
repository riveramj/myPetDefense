package com.mypetdefense.model.domain.reports

import com.mypetdefense.model.{Agency, Status}
import com.mypetdefense.typeclasses.ToCsvStringConverter

import java.util.Date

final case class BasicUsersUpgradeReport(
  activeUpgradeCount: Int,
  inactiveUpgradeCount: Int,
  upgradesByAgency: List[UpgradesByAgency]
) {
  def toCsv: String = {
    val totalUpgrades = activeUpgradeCount + inactiveUpgradeCount
    s"""Total Upgrades,$totalUpgrades
       |,
       |Active Upgrades,$activeUpgradeCount
       |Inactive Upgrades,$inactiveUpgradeCount
       |,
       |Upgrades by Agency
       |Agency,Upgraded Subscriptions,Average Shipment Before Upgrade, Active Upgrades, Active Pets
       |${upgradesByAgency.sortBy(_.totalUpgradedCount).map(_.toCsvRow).mkString("\n")}
       |""".stripMargin
  }
}

final case class BasicUserUpgradeReport(
    subscriptionId: Long,
    userId: Long,
    subscriptionStatus: Status.Value,
    shipmentCountAtUpgrade: Int,
    agency: Option[Agency],
    upgradeDate: Date
)

case class UpgradesByAgency(agencyName: String, totalUpgradedCount: Int, averageShipmentsBeforeUpgrade: Double, activeUpgradeCount: Int, petCount: Int) {
  def toCsvRow = s"$agencyName,$totalUpgradedCount,$averageShipmentsBeforeUpgrade,$activeUpgradeCount,$petCount"
}

object BasicUsersUpgradeReport {
  implicit val toCsvStringConverter: ToCsvStringConverter[BasicUsersUpgradeReport] =
    ToCsvStringConverter.fromFunction(_.toCsv)
}