package com.mypetdefense.model

import com.mypetdefense.util.RandomIdGenerator.generateLongId
import net.liftweb.mapper._

import java.util.Date

class StatisticsSnapshot extends LongKeyedMapper[StatisticsSnapshot] with IdPK with OneToMany[Long, StatisticsSnapshot] {
  def getSingleton: KeyedMetaMapper[Long, StatisticsSnapshot] = StatisticsSnapshot
  object dailySnapshotId extends MappedLong(this) {
    override def dbIndexed_? = true
    override def defaultValue: Long = generateLongId
  }

  object quantity extends MappedInt(this)
  object program  extends MappedEnum(this, BoxType) {
    override def dbIndexed_? = true
  }
  object agency   extends MappedLongForeignKey(this, Agency) {
    override def dbIndexed_? = true
  }
  object date     extends MappedDateTime(this) {
    override def defaultValue = new Date()
    override def dbIndexed_? = true
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object StatisticsSnapshot extends StatisticsSnapshot with LongKeyedMetaMapper[StatisticsSnapshot] {
  def createDailySnapShot(quantity: Int, boxType: BoxType.Value, agency: Agency) = {
    StatisticsSnapshot.create
      .quantity(quantity)
      .program(boxType)
      .agency(agency)
      .saveMe()
  }
}