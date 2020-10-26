package com.mypetdefense.helpers.db

import com.mypetdefense.model.{Agency, AgencyType}
import net.liftweb.common.{Box, Empty}

object AgencyDbUtils {
  def createAgency(
      name: String,
      agencyType: AgencyType.Value = AgencyType.Headquarters,
      parent: Box[Agency] = Empty,
      storeCode: String = "",
      petlandStore: Boolean = false
  ): Agency = Agency.createNewAgency(name, agencyType, parent, storeCode, petlandStore)
}
