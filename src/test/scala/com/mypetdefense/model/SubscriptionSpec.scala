package com.mypetdefense.model

import java.time.ZonedDateTime

import com.mypetdefense.generator.Generator._
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

  private def insertSubscriptionsForTests(
      dataToReturn: Map[UserCreateGeneratedData, SubscriptionCreateGeneratedData],
      dataToIgnore: Map[UserCreateGeneratedData, SubscriptionCreateGeneratedData],
      dataToReturnDate: Option[ZonedDateTime],
      dataToIgnoreDate: Option[ZonedDateTime],
      dataToReturnFun: ZonedDateTime => Subscription => Subscription,
      dataToIgnoreFun: ZonedDateTime => Subscription => Subscription
  ): immutable.Iterable[Subscription] = {
    type DataAndFun =
      (Map[UserCreateGeneratedData, SubscriptionCreateGeneratedData], Subscription => Subscription)

    def makeDataAndFun(
        data: Map[UserCreateGeneratedData, SubscriptionCreateGeneratedData],
        date: Option[ZonedDateTime],
        fun: ZonedDateTime => Subscription => Subscription
    ) =
      date.fold[DataAndFun]((Map.empty, identity))(day => (data, fun(day)))

    val (theReturnData, theReturnFun) =
      makeDataAndFun(dataToReturn, dataToReturnDate, dataToReturnFun)
    val (theIgnoreData, theIgnoreFun) =
      makeDataAndFun(dataToIgnore, dataToIgnoreDate, dataToIgnoreFun)

    insertSubscriptionsForTests(theReturnData, theIgnoreData, theReturnFun, theIgnoreFun)
  }

  it should "find yesterday cancels" in {
    forAll(mapWithNOfUserNSubscriptionGen()) { usersAndYesterdayCanceledSubs =>
      val expectedIds = usersAndYesterdayCanceledSubs.map {
        case (uData, sData) =>
          insertUserAndSub(uData, sData).subscription.cancel
            .cancellationDate(anyHourOfYesterday.toDate)
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
          anyDayOfThisMonthFromTomorrow,
          Some(anyDayOfNextMonth),
          day => sub => sub.nextShipDate(day.toDate).saveMe(),
          day => sub => sub.nextShipDate(day.toDate).saveMe()
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
          _.startDate(anyHourOfToday.toDate).saveMe(),
          _.startDate(anyDayUntilToday.toDate).saveMe()
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
          _.startDate(anyHourOfThisDayMonthAgo.toDate).saveMe(),
          _.startDate(anyDayExceptThisDayMonthAgo.toDate).saveMe()
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
          _.startDate(anyHourOfThisDayYearAgo.toDate).saveMe(),
          _.startDate(anyDayExceptThisDayYearAgo.toDate).saveMe()
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
          _.startDate(anyDayOfThisMonth.toDate).saveMe(),
          _.startDate(anyDayUntilThisMonth.toDate).saveMe()
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
          _.startDate(anyDayOfLastMonthUntilMonthEnd.toDate).saveMe(),
          _.startDate(anyDayUntilLastMonth.toDate).saveMe()
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
          _.startDate(anyDayOfThisYear.toDate).saveMe(),
          _.startDate(anyDayOfLastYear.toDate).saveMe()
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
          anyDayOfThisYearUntilMonthAgo,
          anyDayOfThisYearFromMonthAgo,
          day => sub => sub.startDate(day.toDate).saveMe(),
          day => sub => sub.startDate(day.toDate).saveMe()
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
          Some(anyDayOfLastYearThisDay),
          anyDayOfLastYearFromThisDayYearAgo,
          day => sub => sub.startDate(day.toDate).saveMe(),
          day => sub => sub.startDate(day.toDate).saveMe()
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
          _.cancellationDate(anyDayOfThisMonth.toDate).saveMe(),
          _.cancellationDate(anyDayExceptThisMonth.toDate).saveMe()
        ).map(_.id.get)

        val actualData = Subscription.findCancelledMtdSubscriptions.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

}
