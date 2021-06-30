package com.mypetdefense.snippet.signup

import com.mypetdefense.model.Coupon
import net.liftweb.common.Box

final case class NewUserData(
    email: String,
    firstName: String,
    lastName: String,
    password: String,
    address: NewUserAddress,
    coupon: Box[Coupon],
    ipAddress: String,
    phone: String,
)

final case class NewUserAddress(
    street1: String,
    street2: String,
    city: String,
    state: String,
    zip: String
)
