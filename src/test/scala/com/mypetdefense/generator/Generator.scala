package com.mypetdefense.generator

import com.mypetdefense.helpers.DateUtil.{ZonedDateTimeSyntax, thisMonth}
import com.mypetdefense.model.Price.{
  currentPetland6MonthPaymentCode,
  currentPetlandMonthlyCode,
  currentTppPriceCode,
  defaultPriceCode
}
import com.mypetdefense.model.{AnimalSize, Pet, UserType}
import com.mypetdefense.snippet.signup.{NewUserAddress, NewUserData}
import me.frmr.stripe.{CardList, Customer}
import net.liftweb.common.Empty
import org.scalacheck._

object Generator {

  protected def genAlphaStr: Gen[String] =
    Gen.alphaStr.map(_.take(20))

  protected def genNonEmptyStr: Gen[String] =
    for {
      str <- genAlphaStr
      refinedStr = str + "some"
    } yield refinedStr

  protected def genBool: Gen[Boolean] = Gen.oneOf(true, false)

  protected def genPosLong: Gen[Long] = Gen.posNum[Long]

  protected def genEmailStr: Gen[String] =
    genNonEmptyStr.map(_ + "@foo.com")

  def genUserToCreate: Gen[UserCreateGeneratedData] =
    for {
      firstName <- genNonEmptyStr
      lastName  <- genNonEmptyStr
      stripeId  <- genNonEmptyStr
      email     <- genEmailStr
      password  <- genNonEmptyStr
      phone    = "123-123-1234"
      userType = UserType.Parent
    } yield UserCreateGeneratedData(
      firstName,
      lastName,
      stripeId,
      email,
      password,
      phone,
      Empty,
      Empty,
      Empty,
      userType
    )

  def genSubscriptionToCreate: Gen[SubscriptionCreateGeneratedData] =
    for {
      stripeSubscriptionId <- genNonEmptyStr
      date         = thisMonth
      startDate    = date.toDate
      nextShipDate = date.plusDays(3).toDate
      priceCode      <- Gen.oneOf(defaultPriceCode, currentTppPriceCode)
      contractLength <- Gen.posNum[Int]
    } yield SubscriptionCreateGeneratedData(
      stripeSubscriptionId,
      startDate,
      nextShipDate,
      priceCode,
      contractLength
    )

  def generateStripeCustomer: Gen[Customer] =
    for {
      id       <- genNonEmptyStr
      liveMode <- genBool
      cardList = CardList(List())
      created  <- genPosLong
      aBalance <- genPosLong
      currency = "USD"
      delinquent <- genBool
    } yield Customer(
      id,
      liveMode,
      cardList,
      created,
      aBalance,
      currency,
      delinquent,
      None,
      None,
      None,
      None,
      Map.empty,
      None,
      None
    )

  def generateNewUserAddress: Gen[NewUserAddress] =
    for {
      street1 <- genNonEmptyStr
      street2 <- genNonEmptyStr
      city    <- genNonEmptyStr
      state = "GA"
      zip   = "30312"
    } yield NewUserAddress(street1, street2, city, state, zip)

  def generateNewUserData: Gen[NewUserData] =
    for {
      email     <- genEmailStr
      firstName <- genNonEmptyStr
      lastName  <- genNonEmptyStr
      password  <- genNonEmptyStr
      address   <- generateNewUserAddress
    } yield NewUserData(email, firstName, lastName, password, address, Empty)

  def generateDogOfSupportedSize: Gen[AnimalSize.Value] =
    Gen.oneOf(
      AnimalSize.DogSmallZo,
      AnimalSize.DogMediumZo,
      AnimalSize.DogLargeZo,
      AnimalSize.DogXLargeZo
    )

  def generateSimplePet: Gen[Pet] =
    for {
      name <- genNonEmptyStr
      size <- generateDogOfSupportedSize
    } yield Pet.create.name(name).size(size)

  def userAndSubscriptionGen: Gen[(UserCreateGeneratedData, SubscriptionCreateGeneratedData)] =
    for {
      user <- genUserToCreate
      subs <- genSubscriptionToCreate
    } yield (user, subs)

  def nonEmptyMapUserNSubscriptionGen
      : Gen[Map[UserCreateGeneratedData, SubscriptionCreateGeneratedData]] =
    Gen.nonEmptyMap(userAndSubscriptionGen)

  def nonEmptyUsersGen: Gen[List[UserCreateGeneratedData]] = Gen.nonEmptyListOf(genUserToCreate)

  def nonEmptySimplePetsGen: Gen[List[Pet]] = Gen.nonEmptyListOf(generateSimplePet)

  def stripeCustomer(seed: Long = 42L): Customer =
    generateStripeCustomer.pureApply(Gen.Parameters.default, rng.Seed(seed))

  def newUserData(seed: Long = 42L): NewUserData =
    generateNewUserData.pureApply(Gen.Parameters.default, rng.Seed(seed))

  def simplePetsNonEmptyList(seed: Long = 42L): List[Pet] =
    nonEmptySimplePetsGen.pureApply(Gen.Parameters.default, rng.Seed(seed))

  implicit val arbUserCreation: Arbitrary[UserCreateGeneratedData] = Arbitrary(genUserToCreate)

  implicit val argSubscriptionCreation: Arbitrary[SubscriptionCreateGeneratedData] = Arbitrary(
    genSubscriptionToCreate
  )

}
