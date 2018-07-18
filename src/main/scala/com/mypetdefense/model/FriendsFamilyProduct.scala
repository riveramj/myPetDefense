package com.mypetdefense.model

import net.liftweb.mapper._
import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._

class FriendsFamilyProduct extends LongKeyedMapper[FriendsFamilyProduct] with IdPK with OneToMany[Long, FriendsFamilyProduct] {
  def getSingleton = FriendsFamilyProduct
  object productId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object description extends MappedString(this, 100)
  object sku extends MappedString(this, 100)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createProduct(
    sku: String,
    description: String
  ) = {
    FriendsFamilyProduct.create
    .productId(generateLongId)
    .description(description)
    .sku(sku)
    .saveMe
  }
}

object FriendsFamilyProduct extends FriendsFamilyProduct with LongKeyedMetaMapper[FriendsFamilyProduct]

