package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class Packaging extends LongKeyedMapper[Packaging] with IdPK with OneToMany[Long, Packaging] {
  def getSingleton = Packaging
  object packagingId extends MappedLong(this) {
    override def dbIndexed_? = true
    override def defaultValue = generateLongId
  }
  object name extends MappedString(this, 100)
  object sku extends MappedString(this, 100)
  object treatBagMax extends MappedInt(this)
  object fleaTickMax extends MappedInt(this)
  object weight extends MappedDouble(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewPackaging(
    name: String,
    weight: Double,
    sku: String,
    treatBagMax: Int,
    fleaTickMax: Int
  ) = {
    Packaging.create
      .name(name)
      .weight(weight)
      .sku(sku)
      .treatBagMax(treatBagMax)
      .fleaTickMax(fleaTickMax)
      .saveMe
  }

  def getBubbleMailer = Packaging.find(By(Packaging.sku, "bubble1234"))
  def getSmallBox = Packaging.find(By(Packaging.sku, "smallBox1234"))
  def getLargeBox = Packaging.find(By(Packaging.sku, "largeBox1234"))
}

object Packaging extends Packaging with LongKeyedMetaMapper[Packaging]
