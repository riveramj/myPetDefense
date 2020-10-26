package com.mypetdefense.generator

import com.mypetdefense.helpers.DateUtil.{ZonedDateTimeSyntax, anyDayOfThisMonth}
import com.mypetdefense.model.Price.{
  currentPetland6MonthPaymentCode,
  currentPetlandMonthlyCode,
  currentTppPriceCode,
  defaultPriceCode
}
import com.mypetdefense.model.{AnimalSize, AnimalType, Pet, ShipmentStatus, UserType}
import com.mypetdefense.snippet.signup.{NewUserAddress, NewUserData}
import me.frmr.stripe.{CardList, Customer}
import net.liftweb.common.{Box, Empty}
import org.scalacheck._

object Generator {

  private val STRING_MAX_LENGTH                    = 20
  private val MAX_LENGTH_OF_GENERATED_TRAVERSABLES = 10

  protected def genAlphaStr: Gen[String] =
    Gen.alphaStr.map(_.take(STRING_MAX_LENGTH))

  protected def genMoneyString: Gen[String] = {
    Gen.posNum[Int].map(i => f"${i.toDouble}%2.2f")
  }

  protected def genBoxString: Gen[Box[String]] = Gen.option(genAlphaStr).map(Box.apply[String])

  protected def genNonEmptyStr: Gen[String] =
    for {
      str <- genAlphaStr
      refinedStr = str + "some"
    } yield refinedStr

  protected def genBool: Gen[Boolean] = Gen.oneOf(true, false)

  protected def genPosLong: Gen[Long] = Gen.posNum[Long]

  protected def genEmailStr: Gen[String] =
    genNonEmptyStr.map(_ + "@foo.com")

  def genUserType: Gen[UserType.Value] = Gen.oneOf(UserType.values.toSeq)

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

  def genPriceCode: Gen[String] =
    Gen.oneOf(
      defaultPriceCode,
      currentTppPriceCode,
      currentPetlandMonthlyCode,
      currentPetland6MonthPaymentCode
    )

  def genSubscriptionToCreate: Gen[SubscriptionCreateGeneratedData] =
    for {
      stripeSubscriptionId <- genNonEmptyStr
      date         = anyDayOfThisMonth
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

  def genPetData: Gen[PetData] =
    for {
      petName <- genNonEmptyStr
      size    <- generateDogOfSupportedSize
      petType = AnimalType.Dog
    } yield PetData(petName, petType, size)

  def genPetsChainData: Gen[PetChainData] = {
    for {
      user    <- genUserToCreate
      petData <- Gen.listOfN(5, genPetData)
    } yield PetChainData(user, petData)
  }

  def userAndSubscriptionGen: Gen[(UserCreateGeneratedData, SubscriptionCreateGeneratedData)] =
    for {
      user <- genUserToCreate
      subs <- genSubscriptionToCreate
    } yield (user, subs)

  def genShipmentStatus: Gen[ShipmentStatus.Value] = Gen.oneOf(ShipmentStatus.values.toSeq)

  def genShipmentToCreate: Gen[ShipmentCreateGeneratedData] =
    for {
      stripePaymentId <- genNonEmptyStr
      stripeChargeId  <- genBoxString
      amountPaid      <- genMoneyString
      taxPaid = amountPaid.toDouble - 0.3
      shipmentStatus  <- genShipmentStatus
      sendFreeUpgrade <- genBool
    } yield ShipmentCreateGeneratedData(
      stripePaymentId,
      stripeChargeId,
      amountPaid,
      taxPaid.toString,
      shipmentStatus,
      sendFreeUpgrade
    )

  def genShipmentChainData: Gen[ShipmentChainData] =
    for {
      user     <- genUserToCreate
      sub      <- genSubscriptionToCreate
      shipment <- Gen.listOfN(MAX_LENGTH_OF_GENERATED_TRAVERSABLES, genShipmentToCreate)
    } yield ShipmentChainData(user, sub, shipment)

  def mapWithNOfUserNSubscriptionGen(
      length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[Map[UserCreateGeneratedData, SubscriptionCreateGeneratedData]] =
    Gen.mapOfN(length, userAndSubscriptionGen)

  def listOfNPetsChainDataGen(length: Int): Gen[List[PetChainData]] =
    Gen.listOfN(length, genPetsChainData)

  def listOfNUsersGen(
      length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[List[UserCreateGeneratedData]] = Gen.listOfN(length, genUserToCreate)

  def listOfNSimplePetsGen(length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES): Gen[List[Pet]] =
    Gen.listOfN(length, generateSimplePet)

  def stripeCustomer(seed: Long = 42L): Customer =
    generateStripeCustomer.pureApply(Gen.Parameters.default, rng.Seed(seed))

  def newUserData(seed: Long = 42L): NewUserData =
    generateNewUserData.pureApply(Gen.Parameters.default, rng.Seed(seed))

  def simplePetsNonEmptyList(seed: Long = 42L): List[Pet] =
    listOfNSimplePetsGen().pureApply(Gen.Parameters.default, rng.Seed(seed))

  def listOfNShipmentChainData(
      length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[List[ShipmentChainData]] =
    Gen.listOfN(length, genShipmentChainData)

  implicit val arbUserCreation: Arbitrary[UserCreateGeneratedData] = Arbitrary(genUserToCreate)

  implicit val argSubscriptionCreation: Arbitrary[SubscriptionCreateGeneratedData] = Arbitrary(
    genSubscriptionToCreate
  )

}
