package com.mypetdefense.service

import net.liftweb._
import common._
import mapper._
import util._
import util.Helpers._

import com.mypetdefense.model._

import java.util.Date
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import java.time.{LocalDate, ZoneId, LocalDateTime, Period}
import java.time.format.DateTimeFormatter

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
