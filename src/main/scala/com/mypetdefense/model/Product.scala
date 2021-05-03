package com.mypetdefense.model

import java.util.Date
import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common._
import net.liftweb.mapper._

class Product extends LongKeyedMapper[Product] with IdPK with OneToMany[Long, Product] {
  def getSingleton: KeyedMetaMapper[Long, Product] = Product
  object productId extends MappedLong(this) {
    override def dbIndexed_?        = true
    override def defaultValue: Long = generateLongId
  }
  object name   extends MappedString(this, 100)
  object price  extends MappedDouble(this)
  object sku    extends MappedString(this, 100)
  object weight extends MappedDouble(this)
  object quantity   extends MappedInt(this) {
    override def dbIndexed_? = true
  }
  object animalType extends MappedEnum(this, AnimalType) {
    override def dbIndexed_? = true
  }
  object isSupplement extends MappedBoolean(this) {
    override def dbIndexed_? = true
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def nameAndQuantity = if (this.quantity.get == 0)
    this.name.get
  else
    s"${this.name.get} (${this.quantity.get} ct)"
}

object Product extends Product with LongKeyedMetaMapper[Product] {
  def createNewProduct(
                        name: String,
                        sku: String,
                        quantity: Int,
                        animalType: AnimalType.Value,
                        isSupplement: Boolean
                      ): Product = {
    Product.create
      .name(name)
      .sku(sku)
      .quantity(quantity)
      .animalType(animalType)
      .isSupplement(isSupplement)
      .saveMe
  }

  def hipAndJointForDogs(quantity: Int): Box[Product] =
    Product.find(
      By(Product.name, "Hip & Joint Chews For Dogs"),
      By(Product.quantity, quantity),
      By(Product.animalType, AnimalType.Dog)
    )

  def calmingForDogs(quantity: Int): Box[Product] =
    Product.find(
      By(Product.name, "Calming Chews For Dogs"),
      By(Product.quantity, quantity),
      By(Product.animalType, AnimalType.Dog)
    )

  def multiVitaminForDogs(quantity: Int): Box[Product] =
    Product.find(
      By(Product.name, "Multi-Vitamin Chews For Dogs"),
      By(Product.quantity, quantity),
      By(Product.animalType, AnimalType.Dog)
    )

  def skinAndCoatForDogs(quantity: Int): Box[Product] =
    Product.find(
      By(Product.name, "Skin and Coat Chews For Dogs"),
      By(Product.quantity, quantity),
      By(Product.animalType, AnimalType.Dog)
    )

  def probioticForDogs(quantity: Int): Box[Product] =
    Product.find(
      By(Product.name, "Probiotic Chews For Dogs"),
      By(Product.quantity, quantity),
      By(Product.animalType, AnimalType.Dog)
    )

  def dentalPowderForDogs: Box[Product]      =
    Product.find(
      By(Product.name, "Dental Powder For Dogs"),
      By(Product.animalType, AnimalType.Dog)
    )

  def dentalPowderSmallForDogs: Box[Product] =
    Product.find(
      By(Product.name, "Dental Powder For Dogs (Small)"),
      By(Product.animalType, AnimalType.Dog)
    )

  def dentalPowderLargeForDogs: Box[Product] =
    Product.find(
      By(Product.name, "Dental Powder For Dogs (Large)"),
      By(Product.animalType, AnimalType.Dog)
    )

  def allDentalPowderForDogs: List[Product]  =
    Product.findAll(
      Like(Product.name, "Dental Powder%"),
      By(Product.animalType, AnimalType.Dog)
    )

  def supplementsByAmount(quantity: Int, animalType: AnimalType.Value): List[Product] =
    Product.findAll(
      By(Product.isSupplement, true),
      By(Product.quantity, quantity),
      By(Product.animalType, animalType)
    )

  def allSupplements(animalType: AnimalType.Value): List[Product] =
    Product.findAll(
      By(Product.isSupplement, true),
      By(Product.animalType, animalType)
    )
}
