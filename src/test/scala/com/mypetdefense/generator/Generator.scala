package com.mypetdefense.generator

import com.mypetdefense.helpers.DateUtil.{ZonedDateTimeSyntax, anyDayOfNextMonth, anyDayOfThisMonth}
import com.mypetdefense.helpers.Random.generateMoneyString
import com.mypetdefense.helpers.ScalaCheckHelper
import com.mypetdefense.model.Price._
import com.mypetdefense.model._
import com.mypetdefense.model.domain.action.CustomerAction.{CustomerAddedPet, CustomerRemovedPet}
import com.mypetdefense.model.domain.action.{CustomerAction, CustomerActionSubtype}
import com.mypetdefense.service.{StripeBoxAdapter => Stripe}
import com.mypetdefense.snippet.signup.{NewUserAddress, NewUserData}
import com.stripe.model.{Customer, PaymentSource, PaymentSourceCollection}
import net.liftweb.common.{Box, Empty, Full}
import org.scalacheck._

import java.util.{Collections => JCollections}

object Generator extends ScalaCheckHelper {

  private val ALPHA_STRING_MAX_LENGTH              = 20
  private val NUM_STRING_MAX_LENGTH                = 9
  private val MAX_LENGTH_OF_GENERATED_TRAVERSABLES = 10

  private def boundedStringGen(min: Int, max: Int)(charGen: Gen[Char]): Gen[String] =
    Gen
      .choose(min, max)
      .flatMap(Gen.listOfN(_, charGen))
      .map(_.mkString)
      .suchThat { s =>
        val l = s.length
        min <= l && l <= max
      }

  lazy val genAlphaStr: Gen[String] =
    boundedStringGen(0, ALPHA_STRING_MAX_LENGTH)(Gen.alphaChar)
  lazy val genNonEmptyAlphaStr: Gen[String] =
    boundedStringGen(1, ALPHA_STRING_MAX_LENGTH)(Gen.alphaChar)
  lazy val genNumStr: Gen[String] =
    boundedStringGen(0, NUM_STRING_MAX_LENGTH)(Gen.numChar)
  lazy val genNonEmptyNumStr: Gen[String] =
    boundedStringGen(1, NUM_STRING_MAX_LENGTH)(Gen.numChar)

  lazy val genBool: Gen[Boolean] = Gen.oneOf(true, false)

  private def genNonNegNum[T](implicit num: Numeric[T], c: Gen.Choose[T]): Gen[T] = {
    import num._
    Gen.sized(n => c.choose(zero, max(fromInt(n), one)))
  }
  private def genNonPosNum[T](implicit num: Numeric[T], c: Gen.Choose[T]): Gen[T] =
    genNonNegNum.map(num.negate)

  lazy val genPosLong: Gen[Long]    = Gen.posNum[Long]
  lazy val genNegLong: Gen[Long]    = Gen.negNum[Long]
  lazy val genNonNegLong: Gen[Long] = genNonNegNum[Long]
  lazy val genNonPosLong: Gen[Long] = genNonPosNum[Long]

  lazy val genPosInt: Gen[Int]    = Gen.posNum[Int]
  lazy val genNegInt: Gen[Int]    = Gen.negNum[Int]
  lazy val genNonNegInt: Gen[Int] = genNonNegNum[Int]
  lazy val genNonPosInt: Gen[Int] = genNonPosNum[Int]

  def genMoneyString: Gen[String] = Gen.posNum[Int].map(i => f"${i.toDouble}%2.2f")

  def genBoxString: Gen[Box[String]] = Gen.option(genAlphaStr).map(Box.apply[String])

  def genNonEmptyBoxStr: Gen[Box[String]] = genNonEmptyAlphaStr.map(Full.apply[String])

  def genEmailStr: Gen[String] = genNonEmptyAlphaStr.map(_ + "@foo.com")

  def genSignedNumStr: Gen[String] =
    for {
      num  <- genNonEmptyNumStr
      sign <- Gen.oneOf("-", "")
    } yield s"$sign$num"

  def genUserType: Gen[UserType.Value] = Gen.oneOf(UserType.values.toSeq)

  def genUserToCreate: Gen[UserCreateGeneratedData] =
    for {
      firstName <- genNonEmptyAlphaStr
      lastName  <- genNonEmptyAlphaStr
      stripeId  <- genNonEmptyAlphaStr
      email     <- genEmailStr
      password  <- genNonEmptyAlphaStr
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
      userType,
      ""
    )

  def genPriceCode: Gen[String] =
    Gen.oneOf(
      defaultPriceCode,
      currentTppPriceCode,
      currentPetlandMonthlyCode,
      currentPetland6MonthPaymentCode
    )

  def genInsertData: Gen[InsertGenData] =
    for {
      name       <- genAlphaStr
      itemNumber <- Gen.posNum[Int]
      weight = generateMoneyString.toDouble
    } yield InsertGenData(name, itemNumber, weight)

  def genSubscriptionToCreate: Gen[SubscriptionCreateGeneratedData] =
    for {
      stripeSubscriptionId <- genNonEmptyAlphaStr
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

  def generateStripeCustomer: Gen[Stripe.Customer] =
    for {
      id       <- genNonEmptyAlphaStr
      liveMode <- genBool
      cardList = {
        val c = new PaymentSourceCollection
        c.setData(JCollections.emptyList[PaymentSource])
        c
      }
      created <- genPosLong
      balance <- genPosLong
      currency = "USD"
      delinquent <- genBool
    } yield {
      val c = new Customer
      c.setId(id)
      c.setLivemode(liveMode)
      c.setSources(cardList)
      c.setCreated(created)
      c.setBalance(balance)
      c.setCurrency(currency)
      c.setDelinquent(delinquent)
      Stripe.Customer(c)
    }

  def generateNewUserAddress: Gen[NewUserAddress] =
    for {
      street1 <- genNonEmptyAlphaStr
      street2 <- genNonEmptyAlphaStr
      city    <- genNonEmptyAlphaStr
      state = "GA"
      zip   = "30312"
    } yield NewUserAddress(street1, street2, city, state, zip)

  def generateNewUserData: Gen[NewUserData] =
    for {
      email     <- genEmailStr
      firstName <- genNonEmptyAlphaStr
      lastName  <- genNonEmptyAlphaStr
      password  <- genNonEmptyAlphaStr
      address   <- generateNewUserAddress
    } yield NewUserData(email, firstName, lastName, password, address, Empty, "")

  def generateAddress: Gen[AddressGeneratedData] =
    for {
      street1 <- genNonEmptyAlphaStr
      street2 <- genNonEmptyAlphaStr
      city    <- genNonEmptyAlphaStr
      state = "GA"
      zip   = "30312"
    } yield AddressGeneratedData(street1, street2, city, state, zip, AddressType.Shipping)

  def generateDogOfSupportedSize: Gen[AnimalSize.Value] =
    Gen.oneOf(
      AnimalSize.DogSmallZo,
      AnimalSize.DogMediumZo,
      AnimalSize.DogLargeZo,
      AnimalSize.DogXLargeZo
    )

  def generateSimplePet: Gen[Pet] =
    for {
      name <- genNonEmptyAlphaStr
      size <- generateDogOfSupportedSize
    } yield Pet.create.name(name).size(size)

  def generatePendingPet: Gen[PendingPet] =
    for {
      pet <- generateSimplePet
      supplement <- Gen.oneOf(Product.supplementsByAmount(30, AnimalType.Dog))
    } yield {
      PendingPet(pet, Full(supplement))
    }

  def genPetData: Gen[PetData] =
    for {
      petName <- genNonEmptyAlphaStr
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
      stripePaymentId <- genNonEmptyAlphaStr
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

  def genSubscriptionUpdateToCreate: Gen[SubscriptionUpgradeCreateGeneratedData] =
    for {
      shipmentCountAtUpgrade <- genNonNegInt
    } yield SubscriptionUpgradeCreateGeneratedData(shipmentCountAtUpgrade)

  def genShipmentChainData: Gen[ShipmentChainData] =
    for {
      user     <- genUserToCreate
      sub      <- genSubscriptionToCreate
      shipment <- Gen.listOfN(MAX_LENGTH_OF_GENERATED_TRAVERSABLES, genShipmentToCreate)
    } yield ShipmentChainData(user, sub, shipment)

  def genSubscriptionUpgradeChainData(
      petCount: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[SubscriptionUpgradeChainData] =
    for {
      user     <- genUserToCreate
      sub      <- genSubscriptionToCreate
      subscriptionUpdate <- genSubscriptionUpdateToCreate
      pets <- Gen.listOfN(petCount, genPetData)
    } yield SubscriptionUpgradeChainData(sub, user, pets, subscriptionUpdate)

  def genPetAndShipmentChainData(
      petsSize: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[PetsAndShipmentChainData] =
    for {
      shipmentChainData <- genShipmentChainData
      pets              <- Gen.listOfN(petsSize, genPetData)
    } yield PetsAndShipmentChainData(
      shipmentChainData.user,
      shipmentChainData.subscriptionCreateGeneratedData,
      shipmentChainData.shipmentCreateGeneratedData,
      pets
    )

  def genProduct: Gen[ProductGeneratedData] =
    for {
      productName <- genAlphaStr
      sku         <- genAlphaStr
      quantity    <- genNonNegInt
      isSupplement <- genBool
    } yield ProductGeneratedData(productName, sku, quantity, isSupplement)

  def genProductsSchedule(
      productsSize: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[ProductScheduleGeneratedChainData] =
    for {
      products <- Gen.listOfN(productsSize, genProduct)
      startDate = anyDayOfNextMonth.toDate
    } yield ProductScheduleGeneratedChainData(products, startDate)

  def genStatusLabelCreatedOrPaid: Gen[ShipmentStatus.Value] =
    Gen.oneOf(ShipmentStatus.LabelCreated, ShipmentStatus.Paid)

  def genCustomerAction: Gen[CustomerAction] =
    for {
      subtype <- Gen.oneOf(Set(CustomerActionSubtype.CustomerAddedPet, CustomerActionSubtype.CustomerRemovedPet))
      userId  <- genPosLong
      parentId  <- genPosLong
      petId   <- genPosLong
      petName   <- genAlphaStr
    } yield subtype match {
      case CustomerActionSubtype.CustomerAddedPet   => CustomerAddedPet(parentId, Some(userId), petId, petName)
      case CustomerActionSubtype.CustomerRemovedPet => CustomerRemovedPet(parentId, Some(userId), petId, petName)
    }

  def mapWithNOfUserNSubscriptionGen(
      length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[Map[UserCreateGeneratedData, SubscriptionCreateGeneratedData]] =
    Gen.mapOfN(length, userAndSubscriptionGen)

  def listOfNPetsChainDataGen(
      length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[List[PetChainData]] =
    Gen.listOfN(length, genPetsChainData)

  def listOfNUsersGen(
      length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[List[UserCreateGeneratedData]] = Gen.listOfN(length, genUserToCreate)

  def listOfNSimplePetsGen(length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES): Gen[List[Pet]] =
    Gen.listOfN(length, generateSimplePet)

  def listOfNPendingPetsGen(length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES): Gen[List[PendingPet]] =
    Gen.listOfN(length, generatePendingPet)

  def listOfNInsertDataGen(
      length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[List[InsertGenData]] =
    Gen.listOfN(length, genInsertData)

  def listOfNPosIntsGen(
      length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[List[Int]] =
    Gen.listOfN(length, genPosInt)

  def stripeCustomer(seed: Long = 42L): Stripe.Customer =
    generateStripeCustomer.pureApply(Gen.Parameters.default, rng.Seed(seed))

  def newUserData(seed: Long = 42L): NewUserData =
    generateNewUserData.pureApply(Gen.Parameters.default, rng.Seed(seed))

  def pendingPetsNonEmptyList(seed: Long = 42L): List[PendingPet] =
    listOfNPendingPetsGen().pureApply(Gen.Parameters.default, rng.Seed(seed))

  def subscription(seed: Long = 42L): SubscriptionCreateGeneratedData =
    genSubscriptionToCreate.pureApply(Gen.Parameters.default, rng.Seed(seed))

  def statusLabelCreatedOrPaid(seed: Long = 42L): ShipmentStatus.Value =
    genStatusLabelCreatedOrPaid.pureApply(Gen.Parameters.default, rng.Seed(seed))

  def mapWithNOfUserNSubscription(
      seed: Long = 42L,
      mapSize: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Map[UserCreateGeneratedData, SubscriptionCreateGeneratedData] =
    mapWithNOfUserNSubscriptionGen(mapSize).pureApply(Gen.Parameters.default, rng.Seed(seed))

  def listOfNShipmentChainData(
      seed: Long = 42L,
      listSize: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): List[ShipmentChainData] =
    listOfNShipmentChainDataGen(listSize).pureApply(Gen.Parameters.default, rng.Seed(seed))

  def shipmentChainData(
      seed: Long = 42L
  ): ShipmentChainData =
    genShipmentChainData.pureApply(Gen.Parameters.default, rng.Seed(seed))

  def petsAndShipmentChainDataGen(
      seed: Long = 42L,
      petsSize: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): PetsAndShipmentChainData =
    genPetAndShipmentChainData(petsSize).pureApply(Gen.Parameters.default, rng.Seed(seed))

  def listOfNSubscriptionUpgradeChainData(
    upgradeCount: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES,
    petCount: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[List[SubscriptionUpgradeChainData]] =
    listOfNSubscriptionUpgradeChainDataGen(upgradeCount, petCount)

  def address(seed: Long = 42L): AddressGeneratedData =
    generateAddress.pureApply(Gen.Parameters.default, rng.Seed(seed))

  def insertData(
      seed: Long = 42L,
      listSize: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): List[InsertGenData] =
    listOfNInsertDataGen(listSize).pureApply(Gen.Parameters.default, rng.Seed(seed))

  def productUpdateSchedule(
      seed: Long = 42L,
      productsSize: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): ProductScheduleGeneratedChainData =
    genProductsSchedule(productsSize).pureApply(Gen.Parameters.default, rng.Seed(seed))

  def product(
      seed: Long = 42L
  ): ProductGeneratedData =
    genProduct.pureApply(Gen.Parameters.default, rng.Seed(seed))

  def listOfNShipmentChainDataGen(
      length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[List[ShipmentChainData]] =
    Gen.listOfN(length, genShipmentChainData)

  def listOfNSubscriptionUpgradeChainDataGen(
    length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES,
    petCount: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[List[SubscriptionUpgradeChainData]] =
    Gen.listOfN(length, genSubscriptionUpgradeChainData(petCount))

  def listOfNPetsAndShipmentChainDataGen(
      length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[List[PetsAndShipmentChainData]] =
    Gen.listOfN(length, genPetAndShipmentChainData())

  def listOfNCustomerActions(
      length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[List[CustomerAction]] =
    Gen.listOfN(length, genCustomerAction)

  def listOfSubscriptionToCreateGen(
      length: Int = MAX_LENGTH_OF_GENERATED_TRAVERSABLES
  ): Gen[List[SubscriptionCreateGeneratedData]] =
    Gen.listOfN(length, genSubscriptionToCreate)

  implicit val arbUserCreation: Arbitrary[UserCreateGeneratedData] = Arbitrary(genUserToCreate)

  implicit val argSubscriptionCreation: Arbitrary[SubscriptionCreateGeneratedData] = Arbitrary(
    genSubscriptionToCreate
  )

}
