package com.mypetdefense.model

import com.mypetdefense.generator.Generator.mapWithNOfUserNSubscriptionGen
import com.mypetdefense.generator.{SubscriptionCreateGeneratedData, UserCreateGeneratedData}
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.GeneralDbUtils.insertUserAndSub
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.immutable

class SubscriptionSpec extends DBTest {

  private def insertSubscriptionsForTests(
      dataToReturn: Map[UserCreateGeneratedData, SubscriptionCreateGeneratedData],
      dataToIgnore: Map[UserCreateGeneratedData, SubscriptionCreateGeneratedData],
      dataToReturnFun: Subscription => Subscription,
      dataToIgnoreFun: Subscription => Subscription
  ): immutable.Iterable[Subscription] = {
    dataToIgnore.foreach {
      case (uData, sData) =>
        dataToIgnoreFun(insertUserAndSub(uData, sData).subscription)
    }
    dataToReturn.map {
      case (uData, sData) =>
        dataToReturnFun(insertUserAndSub(uData, sData).subscription)
    }
  }

  it should "find yesterday cancels" in {
    forAll(mapWithNOfUserNSubscriptionGen()) { usersAndYesterdayCanceledSubs =>
      val expectedIds = usersAndYesterdayCanceledSubs.map {
        case (uData, sData) =>
          insertUserAndSub(uData, sData).subscription.cancel
            .cancellationDate(anyHourOfYesterday)
            .saveMe()
            .id
            .get
      }

      val actualData = Subscription.yesterdayCancels.map(_.id.get)

      actualData should contain theSameElementsAs expectedIds
      cleanUpSuccess()
    }
  }

  it should "find current month upcoming subscriptions" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (fromTomorrowData, nextMonthData) =>
        val expectedSubscriptions = insertSubscriptionsForTests(
          fromTomorrowData,
          nextMonthData,
          _.nextShipDate(anyDayOfThisMonthFromTomorrow).saveMe(),
          _.nextShipDate(anyDayOfNextMonth).saveMe()
        ).map(_.id.get)

        val actualData = Subscription.findCurrentMonthUpcomingSubscriptions.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find new today subscriptions" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (todayData, untilTodayData) =>
        val expectedSubscriptions = insertSubscriptionsForTests(
          todayData,
          untilTodayData,
          _.createdAt(anyHourOfToday).saveMe(),
          _.createdAt(anyDayUntilToday).saveMe()
        ).map(_.id.get)

        val actualData = Subscription.findNewTodaySubscriptions.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find new subscriptions for this day in last month" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (thisDayMonthAgoData, anyDayExceptMonthAgoDayData) =>
        val expectedSubscriptions = insertSubscriptionsForTests(
          thisDayMonthAgoData,
          anyDayExceptMonthAgoDayData,
          _.createdAt(anyHourOfThisDayMonthAgo).saveMe(),
          _.createdAt(anyDayExceptThisDayMonthAgo).saveMe()
        ).map(_.id.get)

        val actualData = Subscription.findNewTodaySubscriptionsLastMonth.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find new subscriptions for this day in last year" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (thisDayYearAgoData, anyDayExceptYearAgoDayData) =>
        val expectedSubscriptions = insertSubscriptionsForTests(
          thisDayYearAgoData,
          anyDayExceptYearAgoDayData,
          _.createdAt(anyHourOfThisDayYearAgo).saveMe(),
          _.createdAt(anyDayExceptThisDayYearAgo).saveMe()
        ).map(_.id.get)

        val actualData = Subscription.findNewTodaySubscriptionsLastYear.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find new month subscriptions" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (thisMonthData, anyDayUntilThisMonthData) =>
        val expectedSubscriptions = insertSubscriptionsForTests(
          thisMonthData,
          anyDayUntilThisMonthData,
          _.createdAt(anyDayOfThisMonth).saveMe(),
          _.createdAt(anyDayUntilThisMonth).saveMe()
        ).map(_.id.get)

        val actualData = Subscription.findNewMTDSubscriptions.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find new month subscriptions last month" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (lastMonthData, anyDayUntilLastMonthData) =>
        val expectedSubscriptions = insertSubscriptionsForTests(
          lastMonthData,
          anyDayUntilLastMonthData,
          _.createdAt(anyDayOfLastMonthUntilMonthEnd).saveMe(),
          _.createdAt(anyDayUntilLastMonth).saveMe()
        ).map(_.id.get)

        val actualData = Subscription.findNewMTDSubscriptionsLastMonth.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find new this year subscriptions" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (thisYearData, anyDayLastYearData) =>
        val expectedSubscriptions = insertSubscriptionsForTests(
          thisYearData,
          anyDayLastYearData,
          _.createdAt(anyDayOfThisYear).saveMe(),
          _.createdAt(anyDayOfLastYear).saveMe()
        ).map(_.id.get)

        val actualData = Subscription.findNewYTDSubscriptions.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find new year subscriptions until month ago" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (thisYearDataUntilMonthAgo, anyDayFromMonthAgoData) =>
        val expectedSubscriptions = insertSubscriptionsForTests(
          thisYearDataUntilMonthAgo,
          anyDayFromMonthAgoData,
          _.createdAt(anyDayOfThisYearUntilMonthAgo).saveMe(),
          _.createdAt(anyDayOfThisYearFromMonthAgo).saveMe()
        ).map(_.id.get)

        val actualData = Subscription.findNewYTDSubscriptionsLastMonth.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find new subscriptions in last year until this day year ago" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (thisDayYearAgoData, anyDayExceptThisDayYearAgoData) =>
        val expectedSubscriptions = insertSubscriptionsForTests(
          thisDayYearAgoData,
          anyDayExceptThisDayYearAgoData,
          _.createdAt(anyDayOfLastYearThisDay).saveMe(),
          _.createdAt(anyDayOfLastYearFromThisDayYearAgo).saveMe()
        ).map(_.id.get)

        val actualData = Subscription.findNewYTDSubscriptionsLastYear.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find cancelled month subscriptions" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (thisMonthData, anyMonthExceptThisMonthData) =>
        val expectedSubscriptions = insertSubscriptionsForTests(
          thisMonthData,
          anyMonthExceptThisMonthData,
          _.cancellationDate(anyDayOfThisMonth).saveMe(),
          _.cancellationDate(anyDayExceptThisMonth).saveMe()
        ).map(_.id.get)

        val actualData = Subscription.findCancelledMtdSubscriptions.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

}
