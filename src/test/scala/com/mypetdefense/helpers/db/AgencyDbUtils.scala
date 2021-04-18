package com.mypetdefense.helpers.db

import com.mypetdefense.model.{Agency, AgencyType}
import net.liftweb.common.{Box, Empty}
import net.liftweb.mapper.By

object AgencyDbUtils {
  def createAgency(
      name: String,
      agencyType: AgencyType.Value = AgencyType.Headquarters,
      parent: Box[Agency] = Empty,
      storeCode: String = "",
      petlandStore: Boolean = false
  ): Agency = {
    val possibleAgency = Agency.find(By(Agency.name, name))
    if (possibleAgency.isEmpty)
      Agency.createNewAgency(name, agencyType, parent, storeCode, petlandStore)
    else
      possibleAgency.openOrThrowException("No Agency found or created.")
  }
}
