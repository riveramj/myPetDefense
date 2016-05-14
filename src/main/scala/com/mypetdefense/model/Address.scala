package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import org.apache.shiro.crypto.hash.Sha256Hash
import org.apache.shiro.crypto.SecureRandomNumberGenerator

import java.util.Date

class Address extends LongKeyedMapper[Address] with IdPK {
  def getSingleton = Address
  object addressId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object user extends MappedLongForeignKey(this, User)
  object lead extends MappedLongForeignKey(this, Lead)
  object street1 extends MappedString(this, 100)
  object street2 extends MappedString(this, 100)
  object city extends MappedString(this, 100)
  object state extends MappedString(this, 100)
  object zip extends MappedString(this, 100)
  object addressType extends MappedEnum(this, AddressType)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewAddress(
    user: Box[User],
    lead: Box[Lead],
    street1: String,
    street2: String,
    city: String,
    state: String,
    zip: String,
    addressType: AddressType.Value
  ) = {
    Address.create
    .addressId(generateLongId)
    .user(user)
    .lead(lead)
    .street1(street1)
    .street2(street2)
    .city(city)
    .state(state)
    .zip(zip)
    .addressType(addressType)
    .saveMe
  }
}

object Address extends Address with LongKeyedMetaMapper[Address]

object AddressType extends Enumeration {
  val Shipping, Billing = Value
}
