package com.mypetdefense.model 

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class ItemReconciliation extends LongKeyedMapper[ItemReconciliation] with IdPK with OneToMany[Long, ItemReconciliation] {
  def getSingleton = ItemReconciliation
  object itemReconciliationId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object reconciliationEvent extends MappedLongForeignKey(this, ReconciliationEvent)
  object inventoryItem extends MappedLongForeignKey(this, InventoryItem)
  object expectedCount extends MappedInt(this)
  object actualCount extends MappedInt(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewItemReconciliation(
    inventoryItem: InventoryItem,
    event: ReconciliationEvent,
    actualCount: Int,
    expectedCount: Int
  ) = {
    ItemReconciliation.create
    .itemReconciliationId(generateLongId)
    .reconciliationEvent(event)
    .inventoryItem(inventoryItem)
    .expectedCount(expectedCount)
    .actualCount(actualCount)
    .saveMe
  }
}

object ItemReconciliation extends ItemReconciliation with LongKeyedMetaMapper[ItemReconciliation]
