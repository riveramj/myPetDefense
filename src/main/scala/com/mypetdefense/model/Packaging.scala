package com.mypetdefense.model

import net.liftweb._
import mapper._
import common._
import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class Packaging extends LongKeyedMapper[Packaging] with IdPK with OneToMany[Long, Packaging] {
  def getSingleton: KeyedMetaMapper[Long, Packaging] = Packaging
  object packagingId extends MappedLong(this) {
    override def dbIndexed_?        = true
    override def defaultValue: Long = generateLongId
  }
  object name        extends MappedString(this, 100)
  object sku         extends MappedString(this, 100)
  object treatBagMax extends MappedInt(this)
  object fleaTickMax extends MappedInt(this)
  object weight      extends MappedDouble(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Packaging extends Packaging with LongKeyedMetaMapper[Packaging] {
  def createNewPackaging(
      name: String,
      weight: Double,
      sku: String,
      treatBagMax: Int,
      fleaTickMax: Int
  ): Packaging = {
    Packaging.create
      .name(name)
      .weight(weight)
      .sku(sku)
      .treatBagMax(treatBagMax)
      .fleaTickMax(fleaTickMax)
      .saveMe
  }
  def getBubbleMailer: Box[Packaging] = Packaging.find(By(Packaging.sku, "bubble1234"))
  def getSmallBox: Box[Packaging]     = Packaging.find(By(Packaging.sku, "smallBox1234"))
  def getLargeBox: Box[Packaging]     = Packaging.find(By(Packaging.sku, "largeBox1234"))
}
