package com.mypetdefense.model 

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._
import com.mypetdefense.service.KeyService._
import com.mypetdefense.snippet.NewParent

import org.apache.shiro.crypto.hash.Sha256Hash
import org.apache.shiro.crypto.SecureRandomNumberGenerator

import java.util.Date

class CancelledUser extends LongKeyedMapper[CancelledUser] with IdPK with OneToMany[Long, CancelledUser] {
  def getSingleton = CancelledUser
  object cancelledUserId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  
  object firstName extends MappedString(this, 100)
  object lastName extends MappedString(this, 100)
  object email extends MappedEmail(this, 50)
  object address extends MappedString(this, 100)
  object user extends MappedLongForeignKey(this, User)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def name = s"${firstName} ${lastName}"

  def createNewCancelledUser(
    firstName: String,
    lastName: String,
    email: String,
    address: String,
    userId: Long
  ) = {
    CancelledUser.create
      .cancelledUserId(generateLongId)
      .firstName(firstName)
      .lastName(lastName)
      .email(email)
      .address(address)
      .user(userId)
      .saveMe
  }
}

object CancelledUser extends CancelledUser with LongKeyedMetaMapper[CancelledUser]

