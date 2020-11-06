package com.mypetdefense.model

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator.generateLongId
import net.liftweb.mapper._

class ProductScheduleItem
    extends LongKeyedMapper[ProductScheduleItem]
    with IdPK
    with OneToMany[Long, ProductScheduleItem] {
  override def getSingleton: KeyedMetaMapper[Long, ProductScheduleItem] = ProductScheduleItem

  object productScheduleItemId extends MappedLong(this) {
    override def dbIndexed_?        = true
    override def defaultValue: Long = generateLongId
  }

  object productSchedule extends MappedLongForeignKey(this, ProductSchedule)

  object product extends MappedLongForeignKey(this, Product)

  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object ProductScheduleItem
    extends ProductScheduleItem
    with LongKeyedMetaMapper[ProductScheduleItem] {}
