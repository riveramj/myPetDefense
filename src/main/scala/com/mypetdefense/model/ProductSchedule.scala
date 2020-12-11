package com.mypetdefense.model

import com.mypetdefense.util.DateHelper.tomorrowStart
import com.mypetdefense.util.RandomIdGenerator.generateLongId
import net.liftweb.common.Box
import net.liftweb.mapper._

import java.util.Date

class ProductSchedule
    extends LongKeyedMapper[ProductSchedule]
    with IdPK
    with OneToMany[Long, ProductSchedule] {

  override def getSingleton: KeyedMetaMapper[Long, ProductSchedule] = ProductSchedule

  object productScheduleId extends MappedLong(this) {
    override def dbIndexed_?        = true
    override def defaultValue: Long = generateLongId
  }
  object scheduleStatus extends MappedEnum(this, ProductScheduleStatus) {
    override def defaultValue: ProductScheduleStatus.Value = ProductScheduleStatus.Scheduled
  }
  object scheduledItems extends MappedOneToMany(ProductScheduleItem, ProductScheduleItem.productSchedule)
  object startDate extends MappedDateTime(this)
  object firstBox extends MappedBoolean(this) {
    override def defaultValue: Boolean = false
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def complete: ProductSchedule = this.scheduleStatus(ProductScheduleStatus.Completed).saveMe()
  def makeActive: ProductSchedule = this.scheduleStatus(ProductScheduleStatus.Active).saveMe()
}

object ProductSchedule extends ProductSchedule with LongKeyedMetaMapper[ProductSchedule] {
  def getNextRegularSchedule: Box[ProductSchedule] =
    ProductSchedule
      .find(
        By_<(ProductSchedule.startDate, tomorrowStart),
        By(ProductSchedule.scheduleStatus, ProductScheduleStatus.Scheduled),
        By(ProductSchedule.firstBox, false)
      )

  def getActiveRegularSchedule: Box[ProductSchedule] =
    ProductSchedule.find(
      By(ProductSchedule.scheduleStatus, ProductScheduleStatus.Active),
      By(ProductSchedule.firstBox, false)
    )

  def getNextScheduleForFirstBox: Box[ProductSchedule] =
    ProductSchedule
      .find(
        By_<(ProductSchedule.startDate, tomorrowStart),
        By(ProductSchedule.scheduleStatus, ProductScheduleStatus.Scheduled),
        By(ProductSchedule.firstBox, true)
      )

  def getActiveScheduleForFirstBox: Box[ProductSchedule] =
    ProductSchedule
      .find(
        By(ProductSchedule.scheduleStatus, ProductScheduleStatus.Active),
        By(ProductSchedule.firstBox, true)
      )

  def createNew(
      startDate: Date,
      products: List[Product],
      firstBox: Boolean = false
  ): ProductSchedule = {
    val schedule = ProductSchedule.create.startDate(startDate).firstBox(firstBox).saveMe()
    products.map(ProductScheduleItem.createNew(_, schedule))
    schedule.reload
  }

  def getFirstBoxProducts: List[Product] = {
    ProductSchedule
      .find(
        By(ProductSchedule.scheduleStatus, ProductScheduleStatus.Active),
        By(ProductSchedule.firstBox, true)
      ).toList.flatMap(_.scheduledItems.toList.flatMap(_.product.obj))
  }
}

object ProductScheduleStatus extends Enumeration {
  val Scheduled, Active, Completed = Value
}
