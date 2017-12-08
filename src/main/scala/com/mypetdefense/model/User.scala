package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._
import com.mypetdefense.service.AccessKeyService._

import org.apache.shiro.crypto.hash.Sha256Hash
import org.apache.shiro.crypto.SecureRandomNumberGenerator

import java.util.Date

class User extends LongKeyedMapper[User] with IdPK with OneToMany[Long, User] {
  def getSingleton = User
  object userId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object firstName extends MappedString(this, 100)
  object lastName extends MappedString(this, 100)
  object stripeId extends MappedString(this, 100)
  object email extends MappedEmail(this, 50)
  object password extends MappedString(this, 100)
  object salt extends MappedString(this, 100)
  object phone extends MappedString(this, 100)
  object accessKey extends MappedString(this, 100)
  object resetPasswordKey extends MappedString(this, 100)
  object userType extends MappedEnum(this, UserType)
  object referer extends MappedLongForeignKey(this, Agency)
  object coupon extends MappedLongForeignKey(this, Coupon)
  object agency extends MappedLongForeignKey(this, Agency)
  object survey extends MappedLongForeignKey(this, Survey)
  object pets extends MappedOneToMany(Pet, Pet.user)
  object subscription extends MappedOneToMany(Subscription, Subscription.user)
  object addresses extends MappedOneToMany(Address, Address.user)
  object status extends MappedEnum(this, Status) {
    override def defaultValue = Status.Active
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def name = s"${firstName} ${lastName}"

  def getSubscription = subscription.headOption

  def activePets = pets.filter(_.status == Status.Active)

  def createNewUser(
    firstName: String,
    lastName: String,
    stripeId: String,
    email: String,
    password: String,
    phone: String,
    coupon: Box[Coupon],
    referer: Box[Agency],
    agency: Box[Agency],
    userType: UserType.Value
  ) = {
    val user = User.create
      .userId(generateLongId)
      .firstName(firstName)
      .lastName(lastName)
      .stripeId(stripeId)
      .email(email)
      .phone(phone)
      .coupon(coupon)
      .referer(referer)
      .agency(agency)
      .userType(userType)

    setUserPassword(user, password)
  }

  private def getSalt: String = generateStringId
  
  def hashPassword(password: String, salt: String): String = {
    new Sha256Hash(password, salt, 1024).toBase64
  }

  def setUserPassword(user: User, password: String): User = {
    val salt = getSalt
    val hashedPassword = hashPassword(password, salt)

    user
      .password(hashedPassword)
      .salt(salt)
      .saveMe
  }

  def createNewPendingUser(
    firstName: String,
    lastName: String,
    email: String,
    userType: UserType.Value,
    agency: Box[Agency]
  ) = {
    User.create
      .userId(generateLongId)
      .firstName(firstName)
      .lastName(lastName)
      .email(email)
      .accessKey(createAccessKey)
      .agency(agency)
      .userType(userType)
      .saveMe
  }

  def updatePendingUser(
    user: User,
    firstName: String,
    lastName: String,
    password: String
  ) = {
    val updateduser = user.firstName(firstName).lastName(lastName)

    setUserPassword(updateduser, password)
  }
  
  def findByEmail(email: String): Box[User] = {
    User.find(By(User.email, email))
  }

  def isCorrectPassword_?(password: String, user: User) = {
    user.password.get == hashPassword(password, user.salt.get)
  }

  def nameAndEmail = s"${this.name} <${this.email}>"
}

object User extends User with LongKeyedMetaMapper[User]

object UserType extends Enumeration {
  val Agent, Parent, Admin = Value
}
