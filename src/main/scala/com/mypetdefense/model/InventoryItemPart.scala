package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class InventoryItemPart extends LongKeyedMapper[InventoryItemPart] with IdPK with OneToMany[Long, InventoryItemPart] {
  def getSingleton = InventoryItemPart
  object inventoryItemPartId extends MappedLong(this) {
    override def dbIndexed_? = true
    override def defaultValue = generateLongId
  }
  object finishedItem extends MappedLongForeignKey(this, InventoryItem)
  object itemPart extends MappedLongForeignKey(this, InventoryItem)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewInventoryItemPart(finishedItem: InventoryItem, itemPart: InventoryItem) = {
    InventoryItemPart.create
    .finishedItem(finishedItem)
    .itemPart(itemPart)
    .saveMe
  }
}

object InventoryItemPart extends InventoryItemPart with LongKeyedMetaMapper[InventoryItemPart]
