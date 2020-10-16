package com.mypetdefense.helpers.db

import java.util.Date

import com.mypetdefense.helpers.DateUtil.{lastMonth, lastYear, thisMonth, today}
import com.mypetdefense.helpers.Random.generateString
import com.mypetdefense.model.{Agency, Coupon, User, UserType}
import net.liftweb.common.{Box, Empty}

object UserDbUtils {

  def createUser(
      firstName: String = generateString,
      lastName: String = generateString,
      stripeId: String = generateString,
      email: String = generateString,
      password: String = generateString,
      phone: String = "123-123-1234",
      coupon: Box[Coupon] = Empty,
      referer: Box[Agency] = Empty,
      agency: Box[Agency] = Empty,
      userType: UserType.Value = UserType.Parent
  ): User =
    User.createNewUser(
      firstName,
      lastName,
      stripeId,
      email,
      password,
      phone,
      coupon,
      referer,
      agency,
      userType
    )

  def createRandomUserCreatedAt(createdAt: Date): User = createUser().createdAt(createdAt).saveMe()

  def createTodayUser(): User = createRandomUserCreatedAt(today)

  def createThisMonthUser(): User = createRandomUserCreatedAt(thisMonth)

  def createLastMonthUser(): User = createRandomUserCreatedAt(lastMonth)

  def createLastYearUser(): User = createRandomUserCreatedAt(lastYear)

}
