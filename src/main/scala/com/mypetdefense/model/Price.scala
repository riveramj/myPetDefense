package com.mypetdefense.model

import net.liftweb.mapper._
import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._

class Price extends LongKeyedMapper[Price] with IdPK with OneToMany[Long, Price] {
  def getSingleton = Price
  object priceId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object price extends MappedLong(this)
  object code extends MappedString(this, 100)
  object product extends MappedLongForeignKey(this, Product)
  object active extends MappedBoolean(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createPrice(price: Long, code: String, product: Product) = {
    Price.create
    .priceId(generateLongId)
    .price(price)
    .code(code)
    .product(product)
    .active(true)
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
