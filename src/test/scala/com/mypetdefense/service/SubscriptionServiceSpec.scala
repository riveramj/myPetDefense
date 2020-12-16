package com.mypetdefense.service

import java.time.ZoneId

import com.mypetdefense.generator.Generator.{genShipmentChainData, listOfNShipmentChainDataGen}
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.helpers.DateUtil.{ZonedDateTimeSyntax, anyDayOfThisYear}
import com.mypetdefense.helpers.GeneralDbUtils.insertUserSubAndShipment
import com.mypetdefense.model.Subscription
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class SubscriptionServiceSpec extends DBTest {

  it should "find same day cancels by month" in {
    forAll(listOfNShipmentChainDataGen(), genShipmentChainData, genShipmentChainData) {
      (shouldBeInStatisticData, notCanceledData, canceledAndShippedData) =>
        insertUserSubAndShipment(notCanceledData)
        val shouldBeCanceledAndHaveShipping = insertUserSubAndShipment(canceledAndShippedData)
        shouldBeCanceledAndHaveShipping.subscription.cancel
        shouldBeCanceledAndHaveShipping.shipments.map(
          _.dateShipped(anyDayOfThisYear.toDate).saveMe()
        )

        val expectedData = shouldBeInStatisticData
          .map(insertUserSubAndShipment)
          .map { inserted =>
            inserted.subscription.cancel
            inserted.subscription.createdAt(anyDayOfThisYear.toDate).saveMe()
          }
          .groupBy(_.createdAt.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate.getMonth)
          .mapValues(_.size)

        val subs = Subscription.findAll()

        val actualData = SubscriptionService.sameDayCancelsByMonth(subs)

        actualData should contain theSameElementsAs expectedData
        cleanUpSuccess()
    }
  }

}
