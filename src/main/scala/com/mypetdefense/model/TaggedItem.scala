package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._

class TaggedItem extends LongKeyedMapper[TaggedItem] with IdPK {
  def getSingleton = TaggedItem
  object taggedItemId extends MappedLong(this){
    override def dbIndexed_? = true
    override def defaultValue = generateLongId
  }
  object subscription extends MappedLongForeignKey(this, Subscription)
  object product extends MappedLongForeignKey(this, Product)
  object pet extends MappedLongForeignKey(this, Pet)
  object shipment extends MappedLongForeignKey(this, Shipment)
  object treatOrder extends MappedLongForeignKey(this, TreatOrder)
  object treat extends MappedLongForeignKey(this, Treat)
  object user extends MappedLongForeignKey(this, User)
  object event extends MappedLongForeignKey(this, Event)
  object tag extends MappedLongForeignKey(this, Tag)
  object dateTagged extends MappedDateTime(this)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object TaggedItem extends TaggedItem with LongKeyedMetaMapper[TaggedItem] {
  def createNewTaggedItem(
    user: Box[User] = Empty,
    subscription: Box[Subscription] = Empty,
    shipment: Box[Shipment] = Empty,
    pet: Box[Pet] = Empty,
    product: Box[Product] = Empty,
    treat: Box[Treat] = Empty,
    treatOrder: Box[TreatOrder] = Empty,
    tag: Box[Tag]
  ) = {
    TaggedItem.create
      .taggedItemId(generateLongId)
      .user(user)
      .subscription(subscription)
      .shipment(shipment)
      .pet(pet)
      .treatOrder(treatOrder)
      .treat(treat)
      .tag(tag)
      .dateTagged(new Date())
      .saveMe
  }
}