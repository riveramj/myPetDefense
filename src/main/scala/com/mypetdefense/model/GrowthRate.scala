package com.mypetdefense.model 

import net.liftweb.mapper._
import net.liftweb.common.Box
import java.util.Date
import net.liftweb.util.Props

import com.mypetdefense.util.RandomIdGenerator._

class GrowthRate extends LongKeyedMapper[GrowthRate] with IdPK with OneToMany[Long, GrowthRate] {
  def getSingleton: KeyedMetaMapper[Long, GrowthRate] = GrowthRate
  object growthRateId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object breed extends MappedString(this, 100)
  object mediumProductMonth extends MappedInt(this)
  object largeProductMonth extends MappedInt(this)
  object xlargeProductMonth extends MappedInt(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createGrowthRate(
    breed: String,
    medium: Option[Int] = None,
    large: Option[Int] = None,
    xLarge: Option[Int] = None
  ): GrowthRate = {
    GrowthRate.create
    .growthRateId(generateLongId)
    .breed(breed)
    .mediumProductMonth(medium.getOrElse(-1))
    .largeProductMonth(large.getOrElse(-1))
    .xlargeProductMonth(xLarge.getOrElse(-1))
    .saveMe
  }
}

object GrowthRate extends GrowthRate with LongKeyedMetaMapper[GrowthRate]
