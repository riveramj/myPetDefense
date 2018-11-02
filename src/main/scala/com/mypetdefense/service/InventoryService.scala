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
  def updateItemCount(item: InventoryItem, oldCount: Int, newCount: Int) ={
    InventoryChangeAudit.newChangeAudit(
      inventoryItem = item,
      originalCount = oldCount,
      newCount = newCount
    )

    item.total(newCount).saveMe
  }
  def deductProducts(shipment: Shipment) = {
    val shipmentLineItems = shipment.shipmentLineItems.toList
    
    shipmentLineItems.map { lineItem =>
      for {
        product <- lineItem.product.obj
        itemNumber = product.sku.get
        inventoryItem <- InventoryItem.find(By(InventoryItem.itemNumber, itemNumber))
      } yield {
        val currentCount = inventoryItem.total.get
        updateItemCount(inventoryItem, currentCount, currentCount -1)
      }
    }
  }
}
