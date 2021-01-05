package com.mypetdefense.model

import java.util.Date

import net.liftweb.mapper._

class ActionLog extends LongKeyedMapper[ActionLog] with IdPK with OneToMany[Long, ActionLog] {
  def getSingleton: KeyedMetaMapper[Long, ActionLog] = ActionLog

  object actionType    extends MappedString(this, maxLen = 50)
  object actionSubtype extends MappedString(this, maxLen = 50)
  object details       extends MappedOneToMany(ActionLogDetails, ActionLogDetails.action)
  object user          extends MappedLongForeignKey(this, User)
  object timestamp extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object ActionLog extends ActionLog with LongKeyedMetaMapper[ActionLog]
