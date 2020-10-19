package com.mypetdefense.helpers.db

import com.mypetdefense.generator.UserCreateGeneratedData
import com.mypetdefense.model.User

object UserDbUtils {

  def createUser(data: UserCreateGeneratedData): User =
    User.createNewUser(
      data.firstName,
      data.lastName,
      data.stripeId,
      data.email,
      data.password,
      data.phone,
      data.coupon,
      data.referer,
      data.agency,
      data.userType
    )

}
