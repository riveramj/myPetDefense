package com.mypetdefense.helpers

import bootstrap.liftweb.{DbSetup, MailConfig}
import com.mypetdefense.model.{
  AddOnProduct,
  Address,
  Agency,
  AmazonOrder,
  CancelledUser,
  Coupon,
  Event,
  FleaTick,
  GrowthRate,
  Insert,
  InventoryChangeAudit,
  InventoryItem,
  InventoryItemPart,
  ItemReconciliation,
  Packaging,
  Pet,
  Price,
  Product,
  ReconciliationEvent,
  Review,
  Shipment,
  ShipmentLineItem,
  Subscription,
  SubscriptionBox,
  SubscriptionItem,
  SubscriptionUpgrade,
  Survey,
  Tag,
  TaggedItem,
  TreatOrder,
  TreatOrderLineItem,
  User
}
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
      AmazonOrder
    )
  }

}
