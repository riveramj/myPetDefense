package com.mypetdefense.generator

import com.mypetdefense.helpers.DateUtil.{ZonedDateTimeSyntax, thisMonth}
import com.mypetdefense.model.Price.{
  currentPetland6MonthPaymentCode,
  currentPetlandMonthlyCode,
  currentTppPriceCode,
  defaultPriceCode
}
import com.mypetdefense.model.UserType
import net.liftweb.common.Empty
import org.scalacheck.{Arbitrary, Gen}

object Generator {

  protected def genAlphaStr: Gen[String] =
    Gen.alphaStr.map(_.take(20))

  protected def genNonEmptyStr: Gen[String] =
    for {
      str <- genAlphaStr
      refinedStr = str + "some"
    } yield refinedStr

  protected def genBool: Gen[Boolean] = Gen.oneOf(true, false)

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

  def userAndSubscriptionGen: Gen[(UserCreateGeneratedData, SubscriptionCreateGeneratedData)] =
    for {
      user <- genUserToCreate
      subs <- genSubscriptionToCreate
    } yield (user, subs)

  def nonEmptyMapUserNSubscriptionGen
      : Gen[Map[UserCreateGeneratedData, SubscriptionCreateGeneratedData]] =
    Gen.nonEmptyMap(userAndSubscriptionGen)

  def nonEmptyUsersGen: Gen[List[UserCreateGeneratedData]] = Gen.nonEmptyListOf(genUserToCreate)

  implicit val arbUserCreation: Arbitrary[UserCreateGeneratedData] = Arbitrary(genUserToCreate)

  implicit val argSubscriptionCreation: Arbitrary[SubscriptionCreateGeneratedData] = Arbitrary(
    genSubscriptionToCreate
  )

}
