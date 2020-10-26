package com.mypetdefense.model

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers._
import org.scalatest.{Assertion, BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ShipmentSpec
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach
    with BeforeAndAfterAll
    with ScalaCheckPropertyChecks {
  override def beforeAll() {
    BootUtil.bootForTests()
  }

  override def afterAll(): Unit = {
    clearTables()
  }

  override def afterEach(): Unit = {
    clearTables()
  }

  private def cleanUpSuccess(): Assertion = {
    clearTables()
    succeed
  }

  it should "find mtd shipments" in {
    forAll(genShipmentChainData, genShipmentChainData) { (dataMonth, dataPreviousMonth) =>
      insertUserSubAndShipment(dataPreviousMonth).shipments
        .map(_.createdAt(anyDayUntilThisMonth.toDate).saveMe())
      val expectedShipments = insertUserSubAndShipment(dataMonth).shipments
        .map(_.createdAt(anyDayOfThisMonth.toDate).saveMe().id.get)

      val actualData = Shipment.findMtdShipments.map(_.id.get)

      actualData should contain theSameElementsAs expectedShipments
      cleanUpSuccess()
    }
  }

  it should "find today shipments" in {
    forAll(genShipmentChainData, genShipmentChainData) { (todayData, dataPreviousDays) =>
      insertUserSubAndShipment(dataPreviousDays).shipments
        .map(_.dateProcessed(anyDayUntilToday.toDate).saveMe())
      val expectedShipments = insertUserSubAndShipment(todayData).shipments
        .map(_.dateProcessed(anyHourOfToday.toDate).saveMe().id.get)

      val actualData = Shipment.findTodayShipments.map(_.id.get)

      actualData should contain theSameElementsAs expectedShipments
      cleanUpSuccess()
    }
  }

}
