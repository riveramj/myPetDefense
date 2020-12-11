package com.mypetdefense.model

import java.util.Date

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.mapper._

class CancelledUser
    extends LongKeyedMapper[CancelledUser]
    with IdPK
    with OneToMany[Long, CancelledUser] {
  def getSingleton: KeyedMetaMapper[Long, CancelledUser] = CancelledUser
  object cancelledUserId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object firstName extends MappedString(this, 100)
  object lastName  extends MappedString(this, 100)
  object email     extends MappedEmail(this, 50)
  object address   extends MappedString(this, 100)
  object user      extends MappedLongForeignKey(this, User)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def name = s"${firstName} ${lastName}"

}

object CancelledUser extends CancelledUser with LongKeyedMetaMapper[CancelledUser] {
  def createNewCancelledUser(
      firstName: String,
      lastName: String,
      email: String,
      address: String,
      userId: Long
  ): CancelledUser = {
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
