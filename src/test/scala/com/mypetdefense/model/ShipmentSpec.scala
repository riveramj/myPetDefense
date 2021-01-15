package com.mypetdefense.model

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ShipmentSpec extends DBTest {

  it should "find mtd shipments" in {
    forAll(genShipmentChainData, genShipmentChainData) { (dataMonth, dataPreviousMonth) =>
      insertUserSubAndShipment(dataPreviousMonth).shipments
        .map(_.createdAt(anyDayUntilThisMonth).saveMe())
      val expectedShipments = insertUserSubAndShipment(dataMonth).shipments
        .map(_.createdAt(anyDayOfThisMonth).saveMe().id.get)

      val actualData = Shipment.findMtdShipments.map(_.id.get)

      actualData should contain theSameElementsAs expectedShipments
      cleanUpSuccess()
    }
  }

  it should "find today shipments" in {
    forAll(genShipmentChainData, genShipmentChainData) { (todayData, dataPreviousDays) =>
      insertUserSubAndShipment(dataPreviousDays).shipments
        .map(_.dateProcessed(anyDayUntilToday).saveMe())
      val expectedShipments = insertUserSubAndShipment(todayData).shipments
        .map(_.dateProcessed(anyHourOfToday).saveMe().id.get)

      val actualData = Shipment.findTodayShipments.map(_.id.get)

      actualData should contain theSameElementsAs expectedShipments
      cleanUpSuccess()
    }
  }

}
