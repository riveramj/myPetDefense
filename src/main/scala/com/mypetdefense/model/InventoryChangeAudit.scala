package com.mypetdefense.model

import net.liftweb._
import mapper._
import common._
import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class InventoryChangeAudit
    extends LongKeyedMapper[InventoryChangeAudit]
    with IdPK
    with OneToMany[Long, InventoryChangeAudit] {
  def getSingleton: KeyedMetaMapper[Long, InventoryChangeAudit] = InventoryChangeAudit
  object inventoryChangeAuditId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object inventoryItem         extends MappedLongForeignKey(this, InventoryItem)
  object originalItemNumber    extends MappedString(this, 100)
  object newItemNumber         extends MappedString(this, 100)
  object originalCount         extends MappedInt(this)
  object newCount              extends MappedInt(this)
  object originalDescription   extends MappedString(this, 100)
  object newDescription        extends MappedString(this, 100)
  object originalUnitOfMeasure extends MappedString(this, 100)
  object newUnitOfMeasure      extends MappedString(this, 100)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object InventoryChangeAudit
    extends InventoryChangeAudit
    with LongKeyedMetaMapper[InventoryChangeAudit] {
  def newChangeAudit(
      inventoryItem: InventoryItem,
      originalItemNumber: String = "",
      newItemNumber: String = "",
      originalCount: Int = -1,
      newCount: Int = -1,
      originalDescription: String = "",
      newDescription: String = "",
      originalUnitOfMeasure: String = "",
      newUnitOfMeasure: String = ""
  ): InventoryChangeAudit = {
    InventoryChangeAudit.create
      .inventoryChangeAuditId(generateLongId)
      .inventoryItem(inventoryItem)
      .originalItemNumber(originalItemNumber)
      .newItemNumber(newItemNumber)
      .originalCount(originalCount)
      .newCount(newCount)
      .originalDescription(originalDescription)
      .newDescription(newDescription)
      .originalUnitOfMeasure(originalUnitOfMeasure)
      .newUnitOfMeasure(newUnitOfMeasure)
      .saveMe
  }
}
