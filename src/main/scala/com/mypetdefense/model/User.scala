package com.mypetdefense.model

import java.time.{LocalDate, Month, ZoneId}

import net.liftweb._
import mapper._
import common._
import util._
import Helpers.tryo
import com.mypetdefense.util.RandomIdGenerator._
import com.mypetdefense.service.TaxJarService
import com.mypetdefense.service.KeyService._
import com.mypetdefense.snippet.NewParent
import com.mypetdefense.util.TitleCase
import org.apache.shiro.crypto.hash.Sha256Hash
import java.util.Date

import com.mypetdefense.util.DateHelper.{yesterdayEnd, yesterdayStart}

import scala.collection.mutable

class User extends LongKeyedMapper[User] with IdPK with OneToMany[Long, User] {
  def getSingleton: KeyedMetaMapper[Long, User] = User
  object userId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object firstName  extends MappedString(this, 100)
  object lastName   extends MappedString(this, 100)
  object stripeId   extends MappedString(this, 100)
  object facebookId extends MappedString(this, 300)
  object googleId   extends MappedString(this, 300)
  object email      extends MappedEmail(this, 50)
  object password   extends MappedString(this, 100)
  object salt       extends MappedString(this, 100)
  object phone      extends MappedString(this, 100)
  object accessKey  extends MappedString(this, 100)
  object canSeePetlandData extends MappedBoolean(this) {
    override def defaultValue = false
  }
  object resetPasswordKey extends MappedString(this, 100)
  object productSalesKey  extends MappedString(this, 100)
  object userType         extends MappedEnum(this, UserType)
  object referer          extends MappedLongForeignKey(this, Agency)
  object salesAgentId     extends MappedString(this, 100)
  object coupon           extends MappedLongForeignKey(this, Coupon)
  object agency           extends MappedLongForeignKey(this, Agency)
  object survey           extends MappedLongForeignKey(this, Survey)
  object pets             extends MappedOneToMany(Pet, Pet.user)
  object subscription     extends MappedLongForeignKey(this, Subscription)
  object addresses        extends MappedOneToMany(Address, Address.user)
  object taxRate          extends MappedDouble(this)
  object status extends MappedEnum(this, Status) {
    override def defaultValue: Status.Value = Status.Active
    override def dbIndexed_?                = true
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def name = s"${firstName} ${lastName}"

  def activePets: mutable.Buffer[Pet] = pets.filter(_.status.get == Status.Active)

  def refresh: Box[User] = User.find(By(User.userId, userId.get))

  def petlandData_? : Boolean = tryo(canSeePetlandData.get).openOr(false)

  def shippingAddress: Box[Address] = {
    Address.find(
      By(Address.user, this),
      By(Address.addressType, AddressType.Shipping)
    )
  }

  def upsertUser(
      firstName: String,
      lastName: String,
      email: String,
      password: String,
      facebookId: String,
      userType: UserType.Value
  ): User = {
    val user = (User.find(By(User.email, email)) match {
      case Full(user) => user
      case _          => User.create.userId(generateLongId).productSalesKey(createAccessKey)
    }).firstName(TitleCase(firstName))
      .lastName(TitleCase(lastName))
      .email(email)
      .facebookId(facebookId)
      .userType(userType)

    setUserPassword(user, password)
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
  ): User = {
    val user = User.create
      .userId(generateLongId)
      .firstName(TitleCase(firstName))
      .lastName(TitleCase(lastName))
      .stripeId(stripeId)
      .email(email)
      .productSalesKey(createAccessKey)
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
    val salt           = getSalt
    val hashedPassword = hashPassword(password, salt)

    (if (password != "") {
       user
         .password(hashedPassword)
         .salt(salt)
     } else {
       user.accessKey(createAccessKey)
     }).saveMe
  }

  def createNewPendingUser(
      firstName: String,
      lastName: String,
      email: String,
      userType: UserType.Value,
      agency: Box[Agency],
      referer: Box[Agency],
      salesAgentId: String
  ): User = {
    User.create
      .userId(generateLongId)
      .firstName(TitleCase(firstName))
      .lastName(TitleCase(lastName))
      .email(email)
      .accessKey(createAccessKey)
      .productSalesKey(createAccessKey)
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
  ): User = {
    val possibleLastName = TitleCase(parentInfo.lastName)
    val andLocation      = possibleLastName.indexOf(" And ")

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
      .productSalesKey(createAccessKey)
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
  ): User = {
    val updateduser = user.firstName(TitleCase(firstName)).lastName(TitleCase(lastName))

    setUserPassword(updateduser, password)
  }

  def findByEmail(email: String): Box[User] = {
    User.find(By(User.email, email))
  }

  def findByEmailOrId(email: String, facebookId: String): Box[User] = {
    if (facebookId.isEmpty)
      User.find(By(User.email, email))
    else
      User.find(By(User.facebookId, facebookId))
  }

  def isCorrectPassword_?(password: String, user: User): Boolean = {
    user.password.get == hashPassword(password, user.salt.get)
  }

  def nameAndEmail = s"${this.name} <${this.email}>"

  def cancel: User = {
    val shipAddress = this.addresses.toList.find(_.addressType.get == AddressType.Shipping)

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
      .productSalesKey("")
      .facebookId("")
      .status(Status.Cancelled)
      .saveMe
  }

  def getTaxRate: Double = {
    val shippingAddress = this.shippingAddress

    tryo(
      TaxJarService.calculateTaxRate(
        shippingAddress.map(_.city.get).openOr(""),
        shippingAddress.map(_.state.get).openOr(""),
        shippingAddress.map(_.zip.get).openOr("")
      )
    ).openOr(0d)
  }

  def setTaxRate: User = {
    val rate = getTaxRate

    this.taxRate(rate).saveMe
  }

  def sameDayCancelsByMonth(subscriptions: List[Subscription]): Map[Month, Int] = {
    val sameDayCancels =
      subscriptions
        .filter(_.status.get == Status.Cancelled)
        .filter(_.filterMailedShipments.isEmpty)

    val cancelsByMonth = sameDayCancels.groupBy { subscription =>
      subscription.getCreatedDateOfSubscription.getMonth
    }

    cancelsByMonth.map {
      case (month, subscriptions) =>
        (month, subscriptions.size)
    }
  }

  def getCreatedDateOfUser: LocalDate =
    this.createdAt.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate

  def findYesterdayNewSales: List[User] = {
    User.findAll(
      By_>=(User.createdAt, yesterdayStart),
      By_<(User.createdAt, yesterdayEnd)
    )
  }
}

object User extends User with LongKeyedMetaMapper[User]

object UserType extends Enumeration {
  val Agent, Parent, Admin = Value
}
