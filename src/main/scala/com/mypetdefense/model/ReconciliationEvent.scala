package com.mypetdefense.model 

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import com.mypetdefense.util.TitleCase

import java.util.Date

class ReconciliationEvent extends LongKeyedMapper[ReconciliationEvent] with IdPK with OneToMany[Long, ReconciliationEvent] {
  def getSingleton = ReconciliationEvent
  object reconciliationEventId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object eventDate extends MappedDateTime(this)
  object reconcilations extends MappedOneToMany(ItemReconciliation, ItemReconciliation.reconciliationEvent)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewReconciliationEvent(eventDate: Date) = {
    ReconciliationEvent.create
    .reconciliationEventId(generateLongId)
    .eventDate(eventDate)
    .saveMe
  }
}

object ReconciliationEvent extends ReconciliationEvent with LongKeyedMetaMapper[ReconciliationEvent]

