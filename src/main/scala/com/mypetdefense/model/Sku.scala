package com.mypetdefense.model 

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.TitleCase

import java.util.Date

class Sku extends LongKeyedMapper[Sku] with IdPK with OneToMany[Long, Sku] {
  def getSingleton = Sku
  object skuId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object name extends MappedString(this, 100)
  object sku extends MappedString(this, 100)
  object description extends MappedString(this, 100)
  object total extends MappedInt(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Sku extends Sku with LongKeyedMetaMapper[Sku]
