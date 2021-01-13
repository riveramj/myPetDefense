package com.mypetdefense.model

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common._
import net.liftweb.mapper._

class Tag extends LongKeyedMapper[Tag] with IdPK {
  def getSingleton: KeyedMetaMapper[Long, Tag] = Tag

  object tagId extends MappedLong(this) {
    override def dbIndexed_?        = true
    override def defaultValue: Long = generateLongId
  }
  object name      extends MappedString(this, 100)
  object createdAt extends MappedZonedDateTime(this, useNowAsDefault = true)
}

object Tag extends Tag with LongKeyedMetaMapper[Tag] {
  def createNewTag(name: String): Tag = {
    Tag.create
      .tagId(generateLongId)
      .name(name)
      .saveMe
  }

  def useBox: Box[Tag] = Tag.find(By(Tag.name, "Use Box"))
}
