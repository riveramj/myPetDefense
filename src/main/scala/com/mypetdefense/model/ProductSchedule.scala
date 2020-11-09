package com.mypetdefense.model

import java.util.Date

import com.mypetdefense.util.DateHelper.nowDate
import com.mypetdefense.util.RandomIdGenerator.generateLongId
import net.liftweb.mapper._

class ProductSchedule
    extends LongKeyedMapper[ProductSchedule]
    with IdPK
    with OneToMany[Long, ProductSchedule] {
  override def getSingleton: KeyedMetaMapper[Long, ProductSchedule] = ProductSchedule

  object productScheduleId extends MappedLong(this) {
    override def dbIndexed_?        = true
    override def defaultValue: Long = generateLongId
  }

  object scheduleStatus extends MappedEnum(this, ProductScheduleStatus)

  object scheduledItems
      extends MappedOneToMany(ProductScheduleItem, ProductScheduleItem.productSchedule)

  object startDate extends MappedDateTime(this)

  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

}

object ProductSchedule extends ProductSchedule with LongKeyedMetaMapper[ProductSchedule] {
  def getNextSchedule: Option[ProductSchedule] =
    ProductSchedule
      .findAll(
        By_<=(ProductSchedule.startDate, nowDate),
        By(ProductSchedule.scheduleStatus, ProductScheduleStatus.Scheduled)
      )
      .headOption

  def createNew(startDate: Date, products: List[Product]): ProductSchedule = {
    val schedule = ProductSchedule.create.startDate(startDate).saveMe()
    products.map(ProductScheduleItem.createNew(_, schedule))
    schedule.reload
  }
}

object ProductScheduleStatus extends Enumeration {
  val Scheduled, Completed = Value
}
