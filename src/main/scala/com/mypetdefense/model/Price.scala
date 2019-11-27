package com.mypetdefense.model

import net.liftweb.mapper._
import net.liftweb.common.Box
import java.util.Date
import net.liftweb.util.Props

import com.mypetdefense.util.RandomIdGenerator._

class Price extends LongKeyedMapper[Price] with IdPK with OneToMany[Long, Price] {
  def getSingleton = Price
  object priceId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object price extends MappedDouble(this)
  object code extends MappedString(this, 100)
  object fleaTick extends MappedLongForeignKey(this, FleaTick)
  object active extends MappedBoolean(this)
  object stripeName extends MappedString(this, 200)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createPrice(priceId: Long, price: Double, code: String, fleaTick: FleaTick, stripeName: String) = {
    Price.create
    .priceId(priceId)
    .price(price)
    .code(code)
    .fleaTick(fleaTick)
    .active(true)
    .stripeName(stripeName)
    .saveMe
  }

  def getPricesByCode(fleaTick: FleaTick, code: String, active: Boolean = true) = {
    Price.find(
      By(Price.fleaTick, fleaTick),
      By(Price.code, code),
      By(Price.active, active)
    )
  }

  def getDefaultProductPrice(fleaTick: FleaTick, active: Boolean = true) = {
    Price.find(
      By(Price.fleaTick, fleaTick),
      By(Price.code, Price.defaultPriceCode),
      By(Price.active, active)
    )
  }
}

object Price extends Price with LongKeyedMetaMapper[Price] {
  final val defaultPriceCode = Props.get("default.price.code").openOr("default")
  final val currentTppPriceCode = Props.get("tpp.price.code").openOr(defaultPriceCode)
  final val currentPetland6MonthPaymentCode = Props.get("petland.6month.payment").openOr(defaultPriceCode)
  final val currentPetlandMonthlyCode = Props.get("petland.1month.payment").openOr(defaultPriceCode)
}
