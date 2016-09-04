package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class Coupon extends LongKeyedMapper[Coupon] with IdPK with OneToMany[Long, Coupon] {
  def getSingleton = Coupon
  object couponId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object couponCode extends MappedString(this, 100)
  object freeMonths extends MappedInt(this)
  object referer extends MappedLongForeignKey(this, Agency)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Coupon extends Coupon with LongKeyedMetaMapper[Coupon] {
  def createCoupon(couponCode: String, freeMonths: Int, referer: Box[Agency]) = {
    Coupon.create
      .couponId(generateLongId)
      .couponCode(couponCode.toLowerCase)
      .freeMonths(freeMonths)
      .referer(referer)
      .saveMe
  }
}
