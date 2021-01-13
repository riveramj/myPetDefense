package com.mypetdefense.model

import com.mypetdefense.snippet.NewAddress
import com.mypetdefense.util.RandomIdGenerator._
import com.mypetdefense.util.TitleCase
import net.liftweb.common._
import net.liftweb.mapper._

class Address extends LongKeyedMapper[Address] with IdPK {
  def getSingleton: KeyedMetaMapper[Long, Address] = Address
  object addressId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object user        extends MappedLongForeignKey(this, User)
  object street1     extends MappedString(this, 100)
  object street2     extends MappedString(this, 100)
  object city        extends MappedString(this, 100)
  object state       extends MappedString(this, 100)
  object zip         extends MappedString(this, 100)
  object addressType extends MappedEnum(this, AddressType)
  object status extends MappedEnum(this, Status) {
    override def defaultValue: Status.Value = Status.Active
  }
  object createdAt extends MappedZonedDateTime(this, useNowAsDefault = true)

  def cancel: Address = {
    this
      .street1("")
      .street2("")
      .zip("")
      .status(Status.Cancelled)
      .saveMe
  }
}

object Address extends Address with LongKeyedMetaMapper[Address] {
  def createNewAddress(
      user: Box[User],
      street1: String,
      street2: String,
      city: String,
      state: String,
      zip: String,
      addressType: AddressType.Value
  ): Address = {
    Address.create
      .addressId(generateLongId)
      .user(user)
      .street1(TitleCase(street1))
      .street2(TitleCase(street2))
      .city(TitleCase(city))
      .state(state.toUpperCase)
      .zip(zip)
      .addressType(addressType)
      .saveMe
  }

  def createNewAddress(newAddress: NewAddress, user: Box[User]): Address = {
    Address.create
      .addressId(generateLongId)
      .user(user)
      .street1(TitleCase(newAddress.street1))
      .street2(TitleCase(newAddress.street2.getOrElse("")))
      .city(TitleCase(newAddress.city))
      .state(newAddress.state.toUpperCase)
      .zip(newAddress.zip)
      .addressType(AddressType.Shipping)
      .saveMe
  }
}

object AddressType extends Enumeration {
  val Shipping, Billing = Value
}
