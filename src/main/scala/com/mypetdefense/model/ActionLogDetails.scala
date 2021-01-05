package com.mypetdefense.model

import net.liftweb.mapper._

class ActionLogDetails extends LongKeyedMapper[ActionLogDetails] with IdPK {
  def getSingleton: KeyedMetaMapper[Long, ActionLogDetails] = ActionLogDetails

  object action      extends MappedLongForeignKey(this, ActionLog)
  object key         extends MappedString(this, maxLen = 50)
  object longValue   extends MappedLong(this)
  object stringValue extends MappedString(this, maxLen = 20000)
}

object ActionLogDetails extends ActionLogDetails with LongKeyedMetaMapper[ActionLogDetails]
