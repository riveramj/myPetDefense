package com.mypetdefense.model

import com.mypetdefense.util.RandomIdGenerator.generateLongId

import java.util.Date
import net.liftweb.common.Box
import net.liftweb.mapper._
import net.liftweb.util.Props

class Price extends LongKeyedMapper[Price] with IdPK with OneToMany[Long, Price] {
  def getSingleton: KeyedMetaMapper[Long, Price] = Price
  object priceId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object price      extends MappedDouble(this)
  object code       extends MappedString(this, 100)
  object fleaTick   extends MappedLongForeignKey(this, FleaTick)

  object active     extends MappedBoolean(this)
  object stripeName extends MappedString(this, 200)
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

  def createPrice(
      priceId: Long = generateLongId,
      price: Double,
      code: String,
      fleaTick: Box[FleaTick],
      stripeName: String,
      boxType: Box[BoxType.Value]
  ): Price = {
    Price.create
      .priceId(priceId)
      .price(price)
      .code(code)
      .fleaTick(fleaTick)
      .active(true)
      .stripeName(stripeName)
      .petSize(fleaTick.map(_.size.get).openOrThrowException("Couldn't find pet size"))
      .boxType(boxType.openOrThrowException("Couldn't find box type"))
      .saveMe
  }

  def getPricesByCode(
                       fleaTick: FleaTick,
                       code: String,
                       boxType: BoxType.Value = BoxType.basic,
                       active: Boolean = true
                     ): Box[Price] = {
    Price.find(
      By(Price.fleaTick, fleaTick),
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

  def getDefaultProductPrice(fleaTick: FleaTick, active: Boolean = true): Box[Price] = {
    Price.find(
      By(Price.fleaTick, fleaTick),
      By(Price.code, Price.defaultPriceCode),
      By(Price.active, active)
    )
  }
}
