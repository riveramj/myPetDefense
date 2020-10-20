package com.mypetdefense.helpers

import bootstrap.liftweb.{DbSetup, MailConfig}
import com.mypetdefense.model._
import net.liftweb.mapper.Schemifier

object BootUtil {

  def bootForTests(): Unit = {
    MailConfig.init

    DbSetup.setup

    Schemifier.schemify(
      true,
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
      AmazonOrder,
      ApiRequestBackup
    )
  }

}
