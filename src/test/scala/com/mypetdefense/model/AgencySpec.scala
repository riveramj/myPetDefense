package com.mypetdefense.model

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DateUtil.anyDayOfThisMonth
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers._
import com.mypetdefense.helpers.db.AgencyDbUtils.createAgency
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class AgencySpec extends DBTest {
  it should "find all agency shipments" in {
    forAll(
      genShipmentChainData,
      genShipmentChainData
    ) {
      (agencyShipments, nonAgencyShipments) =>
        val mpdAgency = createAgency(mpdAgencyName)
        val forAgency = insertUserSubAndShipment(agencyShipments)

        forAgency.user.referer(mpdAgency).saveMe()

        insertUserSubAndShipment(nonAgencyShipments)

        val expectedShipments = forAgency.shipments.map(_.id.get)

        val actualData = mpdAgency.reload.findAllShipments.map(_.id.get)

        actualData should contain theSameElementsAs expectedShipments
        cleanUpSuccess()
    }
  }

  it should "find current month agency shipments" in {
    forAll(
      genShipmentChainData,
      genShipmentChainData,
      genShipmentChainData,
    ) {
      (agencyShipmentsThisMonth, agencyShipmentsNotThisMonth, nonAgencyShipmentsThisMonth ) =>
        val mpdAgency = createAgency(mpdAgencyName)

        val forAgencyThisMonth = insertUserSubAndShipment(agencyShipmentsThisMonth)
        val forAgencyLastMonth = insertUserSubAndShipment(agencyShipmentsNotThisMonth)

        forAgencyLastMonth.user.referer(mpdAgency).saveMe()
        forAgencyLastMonth.shipments.map(_.dateShipped(anyDayOfLastMonth.toDate).saveMe())

        insertUserSubAndShipment(nonAgencyShipmentsThisMonth)
          .shipments.map(_.dateShipped(anyDayOfThisMonth.toDate).saveMe())

        forAgencyThisMonth.user.referer(mpdAgency).saveMe()
        val expectedShipments = forAgencyThisMonth
          .shipments.map(_.dateShipped(anyDayOfThisMonth.toDate).saveMe().id.get)

        val actualData = mpdAgency.reload.currentMonthShipments.map(_.id.get)

        actualData should contain theSameElementsAs expectedShipments
        cleanUpSuccess()
    }
  }
}