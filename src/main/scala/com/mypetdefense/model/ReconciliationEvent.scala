package com.mypetdefense.model 

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import com.mypetdefense.util.TitleCase

import java.util.Date

class ReconcilationEvent extends LongKeyedMapper[ReconcilationEvent] with IdPK with OneToMany[Long, ReconcilationEvent] {
  def getSingleton = ReconcilationEvent
  object reconcilationEventId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object eventDate extends MappedDateTime(this)
  object reconcilations extends MappedOneToMany(SkuReconciliation, SkuReconciliation.reconcilationEvent)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object ReconcilationEvent extends ReconcilationEvent with LongKeyedMetaMapper[ReconcilationEvent]

