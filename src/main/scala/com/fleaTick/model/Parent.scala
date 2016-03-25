package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import org.apache.shiro.crypto.hash.Sha256Hash
import org.apache.shiro.crypto.SecureRandomNumberGenerator

import java.util.Date

class Parent extends LongKeyedMapper[Parent] with IdPK {
  def getSingleton = Parent
  object parentId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object firstName extends MappedString(this, 100)
  object lastName extends MappedString(this, 100)
  object email extends MappedEmail(this, 50)
  object password extends MappedString(this, 100)
  object salt extends MappedString(this, 100)
  object street1 extends MappedString(this, 100)
  object street2 extends MappedString(this, 100)
  object city extends MappedString(this, 100)
  object state extends MappedString(this, 100)
  object zip extends MappedString(this, 100)
  object phone extends MappedString(this, 100)
  object admin extends MappedLongForeignKey(this, Admin)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def name = s"${firstName} ${lastName}"

  def createNewParent(
    firstName: String,
    lastName: String,
    email: String,
    password: String,
    street1: String,
    street2: String,
    city: String,
    state: String,
    zip: String,
    phone: String
  ) = {
    val parent =
      Parent.create
      .parentId(generateLongId)
      .firstName(firstName)
      .lastName(lastName)
      .email(email)
      .street1(street1)
      .street2(street2)
      .city(city)
      .state(state)
      .zip(zip)
      .phone(phone)
    
    setParentPassword(parent, password)
  }

  private def getSalt: String = generateStringId
  
  def hashPassword(password: String, salt: String): String = {
    new Sha256Hash(password, salt, 1024).toBase64
  }

  def setParentPassword(Parent: Parent, password: String): Parent = {
    val salt = getSalt
    val hashedPassword = hashPassword(password, salt)

    Parent
      .password(hashedPassword)
      .salt(salt)
      .saveMe
  }
}

object Parent extends Parent with LongKeyedMetaMapper[Parent]
