package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._

class Tag extends LongKeyedMapper[Tag] with IdPK {
  def getSingleton = Tag
  object tagId extends MappedLong(this){
    override def dbIndexed_? = true
    override def defaultValue = generateLongId
  }
  object name extends MappedString(this, 100)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Tag extends Tag with LongKeyedMetaMapper[Tag] {
  def createNewTag(name: String) = {
    Tag.create
      .tagId(generateLongId)
      .name(name)
      .saveMe
  }

  def useBoxTag = Tag.find(By(Tag.name, "Use Box"))
}

