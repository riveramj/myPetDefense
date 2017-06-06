package com.mypetdefense.model

import net.liftweb.mapper._
import net.liftweb.common.Box
import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._

class Price extends LongKeyedMapper[Price] with IdPK with OneToMany[Long, Price] {
  def getSingleton = Price
  object priceId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object price extends MappedDouble(this)
  object code extends MappedString(this, 100)
  object product extends MappedLongForeignKey(this, Product)
  object active extends MappedBoolean(this)
  object stripeName extends MappedString(this, 200)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createPrice(priceId: Long, price: Double, code: String, product: Product, stripeName: String) = {
    Price.create
    .priceId(priceId)
    .price(price)
    .code(code)
    .product(product)
    .active(true)
    .stripeName(stripeName)
    .saveMe
  }

  def getPricesByCode(code: String, active: Boolean = true) = {
    Price.find(By(Price.code, code), By(Price.active, active))
  }

  def getDefaultProductPrice(product: Product, active: Boolean = true) = {
    Price.find(
      By(Price.product, product),
      By(Price.code, "default"),
      By(Price.active, active)
    )
  }
}

object Price extends Price with LongKeyedMetaMapper[Price]
