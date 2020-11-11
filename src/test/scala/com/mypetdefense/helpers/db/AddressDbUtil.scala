package com.mypetdefense.helpers.db

import com.mypetdefense.generator.AddressGeneratedData
import com.mypetdefense.model.{Address, User}
import net.liftweb.common.Full

object AddressDbUtil {

  def createAddress(in: AddressGeneratedData, user: User): Address = {
    Address.createNewAddress(
      Full(user),
      in.street1,
      in.street2,
      in.city,
      in.state,
      in.zip,
      in.addressType
    )
  }

}
