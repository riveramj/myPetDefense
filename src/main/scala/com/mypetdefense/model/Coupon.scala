package com.mypetdefense.model

import net.liftweb._
import mapper._
import common._

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._

class Coupon extends LongKeyedMapper[Coupon] with IdPK with OneToMany[Long, Coupon] {
  def getSingleton: KeyedMetaMapper[Long, Coupon] = Coupon
  object couponId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object couponCode     extends MappedString(this, 100)
  object numberOfMonths extends MappedInt(this)
  object percentOff     extends MappedInt(this)
  object dollarOff      extends MappedInt(this)
  object users          extends MappedOneToMany(User, User.coupon)
  object agency         extends MappedLongForeignKey(this, Agency)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Coupon extends Coupon with LongKeyedMetaMapper[Coupon] {
  def createNewCoupon(
      couponCode: String,
      agency: Box[Agency],
      numberOfMonths: Int = 0,
      percentOff: Int = 0,
      dollarOff: Int = 0
  ): Coupon = {
    Coupon.create
      .couponId(generateLongId)
      .couponCode(couponCode.toLowerCase)
      .numberOfMonths(numberOfMonths)
      .percentOff(percentOff)
      .dollarOff(dollarOff)
      .agency(agency)
      .saveMe
  }

  def halfOffCouponCode  = "50off"
  def firstMonthFreeCode = "100off"

  def halfOffCoupon: Box[Coupon] =
    Coupon.find(By(Coupon.couponCode, halfOffCouponCode))

  def firstMonthFree: Box[Coupon] =
    Coupon.find(By(Coupon.couponCode, firstMonthFreeCode))
}
