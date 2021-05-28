package com.mypetdefense.model

import com.mypetdefense.util.RandomIdGenerator.generateLongId
import net.liftweb.common.Box
import net.liftweb.mapper._
import net.liftweb.util.Props

import java.util.Date

class Price extends LongKeyedMapper[Price] with IdPK with OneToMany[Long, Price] {
  def getSingleton: KeyedMetaMapper[Long, Price] = Price
  object priceId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object price      extends MappedDouble(this)
  object code       extends MappedString(this, 100)
  object fleaTick   extends MappedLongForeignKey(this, FleaTick)
  object active     extends MappedBoolean(this)
  object stripePriceId   extends MappedString(this, 200)
  object stripeProductId extends MappedString(this, 200)
  object petSize extends MappedEnum(this, AnimalSize)
  object boxType extends MappedEnum(this, BoxType)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Price extends Price with LongKeyedMetaMapper[Price] {
  final val defaultPriceCode    = Props.get("default.price.code").openOr("default")
  final val currentTppPriceCode = Props.get("tpp.price.code").openOr(defaultPriceCode)
  final val currentPetland6MonthPaymentCode =
    Props.get("petland.6month.payment").openOr(defaultPriceCode)
  final val currentPetlandMonthlyCode = Props.get("petland.1month.payment").openOr(defaultPriceCode)
  final val fiveDollarBox = Props.get("five.dollar.price.code").openOr("")

  def getFiveDollarPriceCode(animalSize: AnimalSize.Value) = {
    Price.find(
      By(Price.petSize, animalSize),
      By(Price.code, fiveDollarBox)
    )
  }

  def getChangeProduct() = Price.find(By(Price.code, "change"))

  def getStripeProductId(petSize: AnimalSize.Value, boxType: BoxType.Value): Box[String] =
    Price.find(
      By(Price.petSize, petSize),
      By(Price.active, true),
      By(Price.boxType, boxType)
    )
      .map(_.stripeProductId.get)

  def createPrice(
      price: Double,
      code: String,
      fleaTick: FleaTick,
      stripePriceId: String = "",
      boxType: Box[BoxType.Value]
  ): Price = {
    val stripeProductId = boxType.flatMap(getStripeProductId(fleaTick.size.get, _)).openOr("")

    Price.create
      .priceId(generateLongId)
      .price(price)
      .code(code)
      .fleaTick(fleaTick)
      .stripePriceId(stripePriceId)
      .stripeProductId(stripeProductId)
      .active(true)
      .petSize(fleaTick.size.get)
      .boxType(boxType.openOrThrowException("Couldn't find box type"))
      .saveMe
  }

  def getPricesByCode(
                       fleaTick: FleaTick,
                       code: String,
                       boxType: BoxType.Value = BoxType.basic,
                       active: Boolean = true
                     ): Box[Price] = {
    println("prices")
    println(fleaTick)
    println(code)
    println(boxType)
    println(active)
    println("prices")

    Price.find(
      By(Price.petSize, fleaTick.size.get),
      By(Price.code, code),
      By(Price.active, active),
      By(Price.boxType, boxType)
    )
  }

  def getPricesByCodeBySize(
                       code: String,
                       petSize: AnimalSize.Value,
                       boxType: BoxType.Value,
                       active: Boolean = true
                     ): Box[Price] = {
    Price.find(
      By(Price.code, code),
      By(Price.active, active),
      By(Price.boxType, boxType),
      By(Price.petSize, petSize)
    )
  }

  def getDefaultProductPrice(
    size: AnimalSize.Value,
    boxType: BoxType.Value,
    active: Boolean = true): Box[Price] = {
    Price.find(
      By(Price.petSize, size),
      By(Price.code, Price.defaultPriceCode),
      By(Price.boxType, boxType),
      By(Price.active, active)
    )
  }
}
