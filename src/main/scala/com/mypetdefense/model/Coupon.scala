package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._
    import Helpers._

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._

class Coupon extends LongKeyedMapper[Coupon] with IdPK with OneToMany[Long, Coupon] {
  def getSingleton = Coupon
  object couponId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object couponCode extends MappedString(this, 100)
  object freeMonths extends MappedInt(this)
  object users extends MappedOneToMany(User, User.coupon)
  object agency extends MappedLongForeignKey(this, Agency)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Coupon extends Coupon with LongKeyedMetaMapper[Coupon] {
  def createNewCoupon(couponCode: String, freeMonths: Int, agency: Box[Agency]) = {
    Coupon.create
      .couponId(generateLongId)
      .couponCode(couponCode.toLowerCase)
      .freeMonths(freeMonths)
      .agency(agency)
      .saveMe
  }
}
