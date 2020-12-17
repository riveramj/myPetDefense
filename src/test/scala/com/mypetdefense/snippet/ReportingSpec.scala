package com.mypetdefense.snippet

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers.TestTags.PostgresOnlyTest
import com.mypetdefense.model.Subscription
import com.mypetdefense.snippet.admin.Reporting
import com.mypetdefense.util.ProductNameHelper
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ReportingSpec extends DBTest {

  it should "do proper forecasting counts" taggedAs PostgresOnlyTest in {
    val petlandAndTpp    = createPetlandAndMPDAgencies()
    val upcomingStuff    = petsAndShipmentChainDataGen()
    val notUpcomingStuff = petsAndShipmentChainDataGen()
    val insertedExpected =
      insertPetAndShipmentsChainAtAgency(upcomingStuff, petlandAndTpp.mpd, subUpgraded = false)

    insertedExpected.subscription
      .nextShipDate(inReportForecastDefaultRange.toDate)
      .saveMe()
    insertPetAndShipmentsChainAtAgency(
      notUpcomingStuff,
      petlandAndTpp.mpd,
      subUpgraded = false
    ).subscription
      .nextShipDate(notInReportForecastDefaultRange.toDate)
      .saveMe()

    val expectedResult = calculateExpectedResult(insertedExpected.subscription)

    val reportingSnippet = new Reporting
    val result           = reportingSnippet.getSanitizedSortedNames

    result should contain theSameElementsAs expectedResult
  }

  def calculateExpectedResult(subscription: Subscription): Map[String, Int] = {
    val fleaTickNames =
      subscription.reload.subscriptionBoxes.toList.flatMap(_.fleaTick.obj).map(_.getNameAndSize)
    ProductNameHelper
      .sanitizeFleaTickNames(fleaTickNames)
      .groupBy(identity)
      .mapValues(_.size)
  }

}
