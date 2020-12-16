package com.mypetdefense.service

import com.mypetdefense.model._
import net.liftweb.common._
import net.liftweb.mapper._

object InventoryService extends Loggable {
  def updateItemCount(item: InventoryItem, oldCount: Int, newCount: Int): InventoryItem = {
    InventoryChangeAudit.newChangeAudit(
      inventoryItem = item,
      originalCount = oldCount,
      newCount = newCount
    )

    item.total(newCount).saveMe
  }

  def deductShipmentItems(shipment: Shipment): List[InventoryItem] = {
    val shipmentLineItems = shipment.shipmentLineItems.toList

    val products = shipmentLineItems.flatMap(_.fleaTick.obj)
    val inserts  = shipmentLineItems.map(_.insert.toList).flatten

    for {
      product <- products
      itemNumber = product.sku.get
      inventoryItem <- InventoryItem.find(By(InventoryItem.itemNumber, itemNumber))
    } yield {
      val currentCount = inventoryItem.total.get
      updateItemCount(inventoryItem, currentCount, currentCount - 1)
    }

    for {
      insert <- inserts
      itemNumber = insert.itemNumber.get
      inventoryItem <- InventoryItem.find(By(InventoryItem.itemNumber, itemNumber))
    } yield {
      val currentCount = inventoryItem.total.get
      updateItemCount(inventoryItem, currentCount, currentCount - 1)
    }
  }
}
