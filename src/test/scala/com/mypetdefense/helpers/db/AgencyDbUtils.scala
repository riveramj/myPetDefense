package com.mypetdefense.helpers.db

import com.mypetdefense.helpers.Random.generateString
import com.mypetdefense.model.{Agency, AgencyType}
import net.liftweb.common.{Box, Empty}

object AgencyDbUtils {

  def createAgency(
      name: String = generateString,
      agencyType: AgencyType.Value = AgencyType.Headquarters,
      parent: Box[Agency] = Empty,
      storeCode: String = generateString.take(4),
      petlandStore: Boolean = false
  ): Agency = Agency.createNewAgency(name, agencyType, parent, storeCode, petlandStore)

}
