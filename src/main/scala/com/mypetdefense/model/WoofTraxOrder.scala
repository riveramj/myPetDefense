package com.mypetdefense.model

import net.liftweb.mapper._

import java.util.Date

class WoofTraxOrder extends LongKeyedMapper[WoofTraxOrder] with IdPK with OneToMany[Long, WoofTraxOrder] {
  def getSingleton: KeyedMetaMapper[Long, WoofTraxOrder] = WoofTraxOrder
  object woofTraxOrderId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object offerCode      extends MappedString(this, 1000)
  object woofTraxUserId extends MappedString(this, 1000)
  object user           extends MappedLongForeignKey(this, User)
  object orderDate      extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
  object createdAt      extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object WoofTraxOrder extends WoofTraxOrder with LongKeyedMetaMapper[WoofTraxOrder] {
  def createWoofTraxOrder(
      offerCode: String,
      woofTraxUserId: String,
      user: User
                         ): WoofTraxOrder = {
    WoofTraxOrder.create
      .offerCode(offerCode)
      .woofTraxUserId(woofTraxUserId)
      .user(user)
      .saveMe()
  }
}