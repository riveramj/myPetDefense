package com.mypetdefense.model

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb._
import net.liftweb.mapper._

class ReconciliationEvent
    extends LongKeyedMapper[ReconciliationEvent]
    with IdPK
    with OneToMany[Long, ReconciliationEvent] {
  def getSingleton: KeyedMetaMapper[Long, ReconciliationEvent] = ReconciliationEvent
  object reconciliationEventId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object eventDate extends MappedDateTime(this)
  object reconcilations
      extends MappedOneToMany(ItemReconciliation, ItemReconciliation.reconciliationEvent)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewReconciliationEvent(eventDate: Date): ReconciliationEvent = {
    ReconciliationEvent.create
      .reconciliationEventId(generateLongId)
      .eventDate(eventDate)
      .saveMe
  }
}

object ReconciliationEvent extends ReconciliationEvent with LongKeyedMetaMapper[ReconciliationEvent]
