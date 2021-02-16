package com.mypetdefense.generator

import com.mypetdefense.model.{Agency, Coupon, UserType}
import net.liftweb.common.Box

case class UserCreateGeneratedData(
    firstName: String,
    lastName: String,
    stripeId: String,
    email: String,
    password: String,
    phone: String,
    coupon: Box[Coupon],
    referer: Box[Agency],
    agency: Box[Agency],
    userType: UserType.Value,
    ipAddress: String
)
