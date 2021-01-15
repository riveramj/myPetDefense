package com.mypetdefense.model

import java.time.ZonedDateTime

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.mapper._

class ReconciliationEvent
    extends LongKeyedMapper[ReconciliationEvent]
    with IdPK
    with OneToMany[Long, ReconciliationEvent] {

  def getSingleton: KeyedMetaMapper[Long, ReconciliationEvent] = ReconciliationEvent

  object reconciliationEventId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object eventDate extends MappedZonedDateTime(this)
  object reconcilations
      extends MappedOneToMany(ItemReconciliation, ItemReconciliation.reconciliationEvent)
  object createdAt extends MappedZonedDateTime(this, useNowAsDefault = true)
}

object ReconciliationEvent
    extends ReconciliationEvent
    with LongKeyedMetaMapper[ReconciliationEvent] {
  def createNewReconciliationEvent(eventDate: ZonedDateTime): ReconciliationEvent = {
    ReconciliationEvent.create
      .reconciliationEventId(generateLongId)
      .eventDate(eventDate)
      .saveMe
  }
}
