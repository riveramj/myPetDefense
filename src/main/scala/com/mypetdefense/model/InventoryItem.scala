package com.mypetdefense.model

import net.liftweb._
import mapper._
import common._
import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class InventoryItem
    extends LongKeyedMapper[InventoryItem]
    with IdPK
    with OneToMany[Long, InventoryItem] {
  def getSingleton: KeyedMetaMapper[Long, InventoryItem] = InventoryItem
  object inventoryItemId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object itemNumber    extends MappedString(this, 100)
  object description   extends MappedString(this, 100)
  object itemParts     extends MappedOneToMany(InventoryItemPart, InventoryItemPart.finishedItem)
  object unitOfMeasure extends MappedEnum(this, UnitOfMeasure)
  object total         extends MappedInt(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def refresh: Box[InventoryItem] =
    InventoryItem.find(By(InventoryItem.inventoryItemId, inventoryItemId.get))
}

object InventoryItem extends InventoryItem with LongKeyedMetaMapper[InventoryItem] {
  def createNewInventoryItem(
      itemNumber: String,
      description: String,
      total: Int,
      unitOfMeasure: UnitOfMeasure.Value
  ): InventoryItem = {
    InventoryItem.create
      .inventoryItemId(generateLongId)
      .itemNumber(itemNumber)
      .description(description)
      .unitOfMeasure(unitOfMeasure)
      .total(total)
      .saveMe
  }
}

object UnitOfMeasure extends Enumeration {
  val Each, Carton = Value
}
