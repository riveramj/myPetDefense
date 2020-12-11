package com.mypetdefense.model

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common.Box
import net.liftweb.mapper._

class FleaTick extends LongKeyedMapper[FleaTick] with IdPK with OneToMany[Long, FleaTick] {
  def getSingleton: KeyedMetaMapper[Long, FleaTick] = FleaTick
  object fleaTickId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object name        extends MappedString(this, 100)
  object animalType  extends MappedEnum(this, AnimalType)
  object size        extends MappedEnum(this, AnimalSize)
  object sizeName    extends MappedString(this, 100)
  object prices      extends MappedOneToMany(Price, Price.fleaTick)
  object reviews     extends MappedOneToMany(Review, Review.fleaTick)
  object rating      extends MappedDouble(this)
  object reviewCount extends MappedInt(this)
  object imageName   extends MappedString(this, 100)
  object sku         extends MappedString(this, 100)
  object weight      extends MappedDouble(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def getNameAndSize = s"${this.name} ${this.size.toString()} lbs"

  def getSizeAndSizeName = s"${this.sizeName}, ${this.size.toString()} lbs"

  def zoGuardCat: Box[FleaTick]       = FleaTick.find(By(FleaTick.size, AnimalSize.CatAllSize))
  def zoGuardSmallDog: Box[FleaTick]  = FleaTick.find(By(FleaTick.size, AnimalSize.DogSmallZo))
  def zoGuardMediumDog: Box[FleaTick] = FleaTick.find(By(FleaTick.size, AnimalSize.DogMediumZo))
  def zoGuardLargeDog: Box[FleaTick]  = FleaTick.find(By(FleaTick.size, AnimalSize.DogLargeZo))
  def zoGuardXLargeDog: Box[FleaTick] = FleaTick.find(By(FleaTick.size, AnimalSize.DogXLargeZo))

  def isZoGuard_? : Boolean = this.name.get.toLowerCase.contains("zoguard")
}

object FleaTick extends FleaTick with LongKeyedMetaMapper[FleaTick] {
  def createFleaTick(
      name: String,
      animalType: AnimalType.Value,
      size: AnimalSize.Value,
      sizeName: String,
      imageName: String,
      weight: Double,
      sku: String
  ): FleaTick = {
    FleaTick.create
      .fleaTickId(generateLongId)
      .name(name)
      .animalType(animalType)
      .size(size)
      .sizeName(sizeName)
      .imageName(imageName)
      .sku(sku)
      .saveMe
  }
}
