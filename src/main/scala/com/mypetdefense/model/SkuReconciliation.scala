package com.mypetdefense.model 

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class SkuReconciliation extends LongKeyedMapper[SkuReconciliation] with IdPK with OneToMany[Long, SkuReconciliation] {
  def getSingleton = SkuReconciliation
  object skuReconciliationId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object reconciliationEvent extends MappedLongForeignKey(this, ReconciliationEvent)
  object sku extends MappedLongForeignKey(this, Sku)
  object expectedCount extends MappedInt(this)
  object actualCount extends MappedInt(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewSkuReconciliation(
    sku: Sku,
    event: ReconciliationEvent,
    actualCount: Int,
    expectedCount: Int
  ) = {
    SkuReconciliation.create
    .skuReconciliationId(generateLongId)
    .reconciliationEvent(event)
    .sku(sku)
    .expectedCount(expectedCount)
    .actualCount(actualCount)
    .saveMe
  }
}

object SkuReconciliation extends SkuReconciliation with LongKeyedMetaMapper[SkuReconciliation]
