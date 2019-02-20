package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._
    import Helpers.tryo

import com.mypetdefense.util.RandomIdGenerator._
import com.mypetdefense.service.AccessKeyService._
import com.mypetdefense.snippet.NewParent
import com.mypetdefense.util.TitleCase

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
  object canSeePetlandData extends MappedBoolean(this) {
    override def defaultValue = false
  }
  object resetPasswordKey extends MappedString(this, 100)
  object userType extends MappedEnum(this, UserType) {
    override def dbIndexed_? = true
  }
  object referer extends MappedLongForeignKey(this, Agency)
  object salesAgentId extends MappedString(this, 100)
  object coupon extends MappedLongForeignKey(this, Coupon)
  object agency extends MappedLongForeignKey(this, Agency)
  object survey extends MappedLongForeignKey(this, Survey)
  object pets extends MappedOneToMany(Pet, Pet.user)
  object subscription extends MappedOneToMany(Subscription, Subscription.user)
  object addresses extends MappedOneToMany(Address, Address.user)
  object status extends MappedEnum(this, Status) {
    override def defaultValue = Status.Active
    override def dbIndexed_? = true
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def name = s"${firstName} ${lastName}"

  def getSubscription = subscription.headOption

  def activePets = pets.filter(_.status == Status.Active)

  def refresh = User.find(By(User.userId, userId.get))

  def petlandData_? = tryo(canSeePetlandData.get).openOr(false)

  def shippingAddress = {
    Address.find(
      By(Address.user, this),
      By(Address.addressType, AddressType.Shipping)
    )
  }

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
      .firstName(TitleCase(firstName))
      .lastName(TitleCase(lastName))
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

    if (password != "") {
      user
        .password(hashedPassword)
        .salt(salt)
        .saveMe
    } else {
      user.accessKey(createAccessKey).saveMe
    }
  }

  def createNewPendingUser(
    firstName: String,
    lastName: String,
    email: String,
    userType: UserType.Value,
    agency: Box[Agency],
    referer: Box[Agency],
    salesAgentId: String
  ) = {
    User.create
      .userId(generateLongId)
      .firstName(TitleCase(firstName))
      .lastName(TitleCase(lastName))
      .email(email)
      .accessKey(createAccessKey)
      .agency(agency)
      .userType(userType)
      .referer(referer)
      .salesAgentId(salesAgentId)
      .saveMe
  }

  def createNewPendingUser(
    parentInfo: NewParent,
    referer: Box[Agency],
    salesAgentId: String = ""
  ) = {
    val possibleLastName = TitleCase(parentInfo.lastName)
    val andLocation = possibleLastName.indexOf(" And ")

    val lastName = {
      if (andLocation > 0)
        possibleLastName.substring(0, andLocation)
      else
        possibleLastName
    }

    User.create
      .userId(generateLongId)
      .firstName(TitleCase(parentInfo.firstName))
      .lastName(lastName)
      .email(parentInfo.email)
      .phone(parentInfo.phone.getOrElse(""))
      .accessKey(createAccessKey)
      .userType(UserType.Parent)
      .referer(referer)
      .salesAgentId(salesAgentId)
      .saveMe
  }

  def updatePendingUser(
    user: User,
    firstName: String,
    lastName: String,
    password: String
  ) = {
    val updateduser = user.firstName(TitleCase(firstName)).lastName(TitleCase(lastName))

    setUserPassword(updateduser, password)
  }
  
  def findByEmail(email: String): Box[User] = {
    User.find(By(User.email, email))
  }

  def isCorrectPassword_?(password: String, user: User) = {
    user.password.get == hashPassword(password, user.salt.get)
  }

  def nameAndEmail = s"${this.name} <${this.email}>"

  def cancel = {
    val shipAddress = this.addresses.toList.filter(_.addressType == AddressType.Shipping).headOption

    val address = shipAddress.map { ship =>
      s"""${ship.street1}
      |${ship.street2}
      |${ship.city}, ${ship.state} ${ship.zip}""".stripMargin.replaceAll("\n\n", "\n")
    }.getOrElse("")

    CancelledUser.createNewCancelledUser(
      this.firstName.get,
      this.lastName.get,
      this.email.get,
      address,
      this.userId.get
    )

    this
      .firstName("")
      .lastName("")
      .email("")
      .password("")
      .salt("")
      .phone("")
      .accessKey("")
      .resetPasswordKey("")
      .status(Status.Cancelled)
      .saveMe
  }
}

object User extends User with LongKeyedMetaMapper[User]

object UserType extends Enumeration {
  val Agent, Parent, Admin = Value
}
