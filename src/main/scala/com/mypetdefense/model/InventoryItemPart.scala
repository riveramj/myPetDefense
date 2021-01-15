package com.mypetdefense.model

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.mapper._

class InventoryItemPart
    extends LongKeyedMapper[InventoryItemPart]
    with IdPK
    with OneToMany[Long, InventoryItemPart] {
  def getSingleton: KeyedMetaMapper[Long, InventoryItemPart] = InventoryItemPart

  object inventoryItemPartId extends MappedLong(this) {
    override def dbIndexed_?        = true
    override def defaultValue: Long = generateLongId
  }
  object finishedItem extends MappedLongForeignKey(this, InventoryItem)
  object itemPart     extends MappedLongForeignKey(this, InventoryItem)
  object createdAt    extends MappedZonedDateTime(this, useNowAsDefault = true)
}

object InventoryItemPart extends InventoryItemPart with LongKeyedMetaMapper[InventoryItemPart] {
  def createNewInventoryItemPart(
      finishedItem: InventoryItem,
      itemPart: InventoryItem
  ): InventoryItemPart = {
    InventoryItemPart.create
      .finishedItem(finishedItem)
      .itemPart(itemPart)
      .saveMe
  }
}
