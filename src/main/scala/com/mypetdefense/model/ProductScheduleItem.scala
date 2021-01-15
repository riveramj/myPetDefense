package com.mypetdefense.model

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
  object product         extends MappedLongForeignKey(this, Product)
  object createdAt       extends MappedZonedDateTime(this, useNowAsDefault = true)
}

object ProductScheduleItem
    extends ProductScheduleItem
    with LongKeyedMetaMapper[ProductScheduleItem] {
  def createNew(product: Product, schedule: ProductSchedule): ProductScheduleItem =
    ProductScheduleItem.create.product(product).productSchedule(schedule).saveMe()
}
