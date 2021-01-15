package com.mypetdefense.model

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.mapper._

class ItemReconciliation
    extends LongKeyedMapper[ItemReconciliation]
    with IdPK
    with OneToMany[Long, ItemReconciliation] {
  def getSingleton: KeyedMetaMapper[Long, ItemReconciliation] = ItemReconciliation
  object itemReconciliationId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object reconciliationEvent extends MappedLongForeignKey(this, ReconciliationEvent)
  object inventoryItem       extends MappedLongForeignKey(this, InventoryItem)
  object expectedCount       extends MappedInt(this)
  object actualCount         extends MappedInt(this)
  object createdAt           extends MappedZonedDateTime(this, useNowAsDefault = true)
}

object ItemReconciliation extends ItemReconciliation with LongKeyedMetaMapper[ItemReconciliation] {
  def createNewItemReconciliation(
      inventoryItem: InventoryItem,
      event: ReconciliationEvent,
      actualCount: Int,
      expectedCount: Int
  ): ItemReconciliation = {
    ItemReconciliation.create
      .itemReconciliationId(generateLongId)
      .reconciliationEvent(event)
      .inventoryItem(inventoryItem)
      .expectedCount(expectedCount)
      .actualCount(actualCount)
      .saveMe
  }
}
