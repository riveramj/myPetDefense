package com.mypetdefense.generator

import com.mypetdefense.model.AddressType

case class AddressGeneratedData(
    street1: String,
    street2: String,
    city: String,
    state: String,
    zip: String,
    addressType: AddressType.Value
)
