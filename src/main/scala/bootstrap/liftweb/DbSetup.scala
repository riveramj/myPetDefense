package bootstrap.liftweb

import com.mypetdefense.model._
import net.liftweb.common._
import net.liftweb.http.LiftRules
import net.liftweb.mapper._
import net.liftweb.util
import net.liftweb.util.Props

object DbSetup extends Loggable {

  def setup = {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
        new StandardDBVendor(
          Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"),
          Props.get("db.password")
        )

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(util.DefaultConnectionIdentifier, vendor)
    }
  }

  def migrateTables: List[String] = Schemifier.schemify(
    performWrite = true,
    Schemifier.infoF _,
    User,
    CancelledUser,
    Address,
    Pet,
    FleaTick,
    Shipment,
    Event,
    ShipmentLineItem,
    Subscription,
    SubscriptionBox,
    SubscriptionItem,
    SubscriptionUpgrade,
    Agency,
    Coupon,
    Price,
    GrowthRate,
    Review,
    Survey,
    TreatOrder,
    ItemReconciliation,
    ReconciliationEvent,
    InventoryItem,
    InventoryChangeAudit,
    Insert,
    InventoryItemPart,
    Product,
    TreatOrderLineItem,
    Packaging,
    TaggedItem,
    Tag,
    AddOnProduct,
    ApiRequestBackup,
    ProductSchedule,
    ProductScheduleItem,
    ActionLog,
    ActionLogDetails,
    WoofTraxOrder,
    EmailReport,
    EmailReportRecord,
    StatisticsSnapshot
  )

}
