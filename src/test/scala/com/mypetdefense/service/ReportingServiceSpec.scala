package com.mypetdefense.service

import com.mypetdefense.generator.Generator._
import com.mypetdefense.generator.{SubscriptionCreateGeneratedData, UserCreateGeneratedData}
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers._
import com.mypetdefense.helpers.db.UserDbUtils._
import com.mypetdefense.helpers.db.AgencyDbUtils._
import com.mypetdefense.model._
import org.scalatest.{Assertion, BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ReportingServiceSpec
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

  private def createUserAndSubReturnSubId(
      t: (UserCreateGeneratedData, SubscriptionCreateGeneratedData)
  ): Long =
    insertUserAndSub(t._1, t._2).subscription.id.get

  private def createUserAndCancelSubReturnSubId(
      t: (UserCreateGeneratedData, SubscriptionCreateGeneratedData)
  ): Long =
    insertUserAndSub(t._1, t._2).subscription.cancel.saveMe().id.get

  it should "find all active subscriptions" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (usersWithActiveSub, usersWithoutSub) =>
        val activeSubsIds = usersWithActiveSub.map(createUserAndSubReturnSubId)
        usersWithoutSub.foreach(createUserAndCancelSubReturnSubId)

        val subscriptions   = Subscription.findAll()
        val filteredSubsIds = ReportingService.findActiveSubscriptions(subscriptions).map(_.id.get)

        filteredSubsIds should contain theSameElementsAs activeSubsIds
        cleanUpSuccess()
    }
  }

  it should "find customers for agent" in {
    forAll(listOfNUsersGen(), listOfNUsersGen()) { (agentUsers, notAgentUsers) =>
      val agentId       = Random.generateString.take(10)
      val agentUsersIds = agentUsers.map(d => createUser(d).salesAgentId(agentId).saveMe().id.get)
      notAgentUsers.foreach(createUser)

      val customers            = User.findAll()
      val customersForAgent    = ReportingService.findCustomersForAgent(customers, agentId)
      val customersForAgentIds = customersForAgent.map(_.id.get)

      customersForAgent.head.salesAgentId.get shouldBe agentId
      customersForAgentIds should contain theSameElementsAs agentUsersIds
      cleanUpSuccess()
    }
  }

  it should "find new customers for month" in {
    forAll(listOfNUsersGen(), listOfNUsersGen()) { (registeredThisMonth, registeredLongTimeAgo) =>
      val expectedUsersIds =
        registeredThisMonth.map(createUser(_).createdAt(anyDayOfThisMonth.toDate).saveMe().id.get)
      registeredLongTimeAgo.foreach(createUser(_).createdAt(anyDayOfLastMonth.toDate).saveMe())

      val users = User.findAll()
      val usersIdsRegisteredInThisMonth =
        ReportingService
          .findNewCustomersMonth(users, now.getMonth.toString, now.getYear)
          .map(_.id.get)

      usersIdsRegisteredInThisMonth should contain theSameElementsAs expectedUsersIds
      cleanUpSuccess()
    }
  }

  it should "find active subscriptions first month" in {
    forAll(
      mapWithNOfUserNSubscriptionGen(),
      mapWithNOfUserNSubscriptionGen(),
      mapWithNOfUserNSubscriptionGen()
    ) { (activeFirstMonthUsers, canceledLongTimeAgoUsers, canceledThisMonthUsers) =>
      canceledLongTimeAgoUsers.foreach {
        case (u, s) =>
          val date = lastYear
          insertUserAndSub(u, s).subscription
            .createdAt(date.toDate)
            .cancellationDate(date.plusMonths(3).toDate)
            .saveMe()
      }
      val activeFirstMonthUsersIds = activeFirstMonthUsers.map {
        case (u, s) =>
          val thisMonthDay = anyDayOfThisMonth
          insertUserAndSub(u, s).subscription
            .createdAt(thisMonthDay.toDate)
            .saveMe()
            .id
            .get
      }
      val startedAndCanceledThisMonthUsersIds = canceledThisMonthUsers.map {
        case (u, s) =>
          val thisMonthDayDate = anyDayOfThisMonth.withDayOfMonth(1)
          insertUserAndSub(u, s).subscription
            .createdAt(thisMonthDayDate.toDate)
            .cancellationDate(thisMonthDayDate.plusDays(2).toDate)
            .saveMe()
            .id
            .get
      }
      val expectedUsers = activeFirstMonthUsersIds ++ startedAndCanceledThisMonthUsersIds

      val subscriptions = Subscription.findAll()
      val result        = ReportingService.findActiveSubscriptionsFirstMonth(subscriptions).map(_.id.get)

      result should contain theSameElementsAs expectedUsers
      cleanUpSuccess()
    }
  }

  it should "find current month subscriptions" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (currentMonth, oldOne) =>
        oldOne.foreach {
          case (u, s) =>
            insertUserAndSub(u, s).subscription.createdAt(anyDayOfLastMonth.toDate).saveMe()
        }
        val expectedCurrentMonthIds = currentMonth.map(createUserAndSubReturnSubId)

        val subscriptions = Subscription.findAll()
        val result        = ReportingService.findCurrentMonthSubscriptions(subscriptions).map(_.id.get)

        result should contain theSameElementsAs expectedCurrentMonthIds
        cleanUpSuccess()
    }
  }

  it should "find paid shipments" in {
    forAll(genShipmentChainData, genShipmentChainData) {
      (dataWithPaidShipments, dataWithUnpaidShipments) =>
        val paidShipmentsIds =
          insertUserSubAndShipment(dataWithPaidShipments).shipments
            .map(_.dateShipped(today).saveMe().id.get)
        insertUserSubAndShipment(dataWithUnpaidShipments).shipments
          .foreach(_.taxPaid("0").amountPaid("0").saveMe())

        val subscriptions = Subscription.findAll()
        val result        = ReportingService.findPaidShipments(subscriptions).map(_.id.get)

        result should contain theSameElementsAs paidShipmentsIds
        cleanUpSuccess()
    }
  }

  it should "find cancelled subscriptions" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (usersWithCanceledSub, usersWithoutSub) =>
        val canceledIds = usersWithCanceledSub.map(createUserAndCancelSubReturnSubId)
        usersWithoutSub.foreach(createUserAndSubReturnSubId)

        val subscriptions = Subscription.findAll()
        val filteredSubsIds =
          ReportingService.findCancelledSubscriptions(subscriptions).map(_.id.get)

        filteredSubsIds should contain theSameElementsAs canceledIds
        cleanUpSuccess()
    }
  }

  it should "find current year paying cancelled subscriptions" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (cancelledInCurrentYear, cancelledYearAgo) =>
        val cancelledIds = cancelledInCurrentYear.map {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription.cancel
              .cancellationDate(anyDayUntilThisMonth.toDate)
              .saveMe()
              .id
              .get
        }
        cancelledYearAgo.foreach {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription.cancel
              .cancellationDate(anyDayOfLastYear.toDate)
              .saveMe()
        }

        val subs = Subscription.findAll()
        val filteredSubsIds =
          ReportingService.findCurrentYearPayingCancelledSubscriptions(subs).map(_.id.get)

        filteredSubsIds should contain theSameElementsAs cancelledIds
        cleanUpSuccess()
    }
  }

  it should "find current month cancelled subscriptions" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (cancelledThisMonth, cancelledNotThisMonth) =>
        cancelledNotThisMonth.foreach {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription.cancel
              .cancellationDate(anyDayUntilThisMonth.toDate)
              .saveMe()
        }
        val cancelledIds = cancelledThisMonth.map {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription.cancel
              .cancellationDate(anyDayOfThisMonth.toDate)
              .saveMe()
              .id
              .get
        }

        val subs = Subscription.findAll()
        val filteredSubsIds =
          ReportingService
            .findCurrentMonthCancelledSubscriptions(subs, year = thisYear)
            .map(_.id.get)

        filteredSubsIds should contain theSameElementsAs cancelledIds
        cleanUpSuccess()
    }
  }

  it should "find current month processed shipments" in {
    forAll(genShipmentChainData, genShipmentChainData) {
      (currentMonthProcessed, notInCurrentMonthProcessed) =>
        insertUserSubAndShipment(notInCurrentMonthProcessed).shipments
          .foreach(_.dateProcessed(anyDayOfLastYear.toDate).saveMe())
        val processedThisMonthIds = insertUserSubAndShipment(currentMonthProcessed).shipments
          .map(_.dateProcessed(anyDayOfThisMonth.toDate).saveMe().id.get)

        val shipments = Shipment.findAll()
        val filteredShipmentsIds =
          ReportingService
            .findCurrentMonthProcessedShipments(shipments, year = thisYear)
            .map(_.id.get)

        filteredShipmentsIds should contain theSameElementsAs processedThisMonthIds
        cleanUpSuccess()
    }
  }

  it should "find current month shipments" in {
    forAll(genShipmentChainData, genShipmentChainData) {
      (currentMonthShipments, notInCurrentMonthShipments) =>
        insertUserSubAndShipment(notInCurrentMonthShipments).shipments
          .foreach(_.dateShipped(anyDayUntilThisMonth.toDate).saveMe())
        val thisMonthShipmentsIds = insertUserSubAndShipment(currentMonthShipments).shipments
          .map(_.dateShipped(anyDayOfThisMonth.toDate).saveMe().id.get)

        val shipments = Shipment.findAll()
        val filteredShipmentsIds =
          ReportingService
            .findCurrentMonthShipments(shipments)
            .map(_.id.get)

        filteredShipmentsIds should contain theSameElementsAs thisMonthShipmentsIds
        cleanUpSuccess()
    }
  }

  it should "find current year shipments" in {
    forAll(genShipmentChainData, genShipmentChainData) {
      (currentYearShipments, notInCurrentYearShipments) =>
        insertUserSubAndShipment(notInCurrentYearShipments).shipments
          .foreach(_.dateShipped(anyDayOfLastYear.toDate).saveMe())
        val currentYearShipmentsIds = insertUserSubAndShipment(currentYearShipments).shipments
          .map(_.dateShipped(anyDayOfThisYear.toDate).saveMe().id.get)

        val shipments = Shipment.findAll()
        val filteredShipmentsIds =
          ReportingService
            .findCurrentYearShipments(shipments)
            .map(_.id.get)

        filteredShipmentsIds should contain theSameElementsAs currentYearShipmentsIds
        cleanUpSuccess()
    }
  }

  it should "find yesterday new sales" in {
    forAll(listOfNUsersGen()) { users2Create =>
      val expectedIds =
        users2Create.map(createUser).map(_.createdAt(anyHourOfYesterday.toDate).saveMe().id.get)
      val actualIds = ReportingService.findYesterdayNewSales.map(_.id.get)

      actualIds should contain theSameElementsAs expectedIds
      cleanUpSuccess()
    }
  }

  it should "find yesterday shipments" in {
    forAll(genShipmentChainData, genShipmentChainData) { (withPaidShipments, newShipments) =>
      val paidShipments = insertUserSubAndShipment(withPaidShipments).shipments
        .map(_.dateShipped(anyHourOfYesterday.toDate).saveMe())
      val newShipmentsSize =
        insertUserSubAndShipment(newShipments).shipments
          .map(_.dateShipped(anyHourOfYesterday.toDate).saveMe())
          .map(_.amountPaid("0").saveMe())
          .size
      val expectedTotal =
        paidShipments.foldLeft(0d)((acc, s) =>
          (s.amountPaid.get.toDouble - s.taxPaid.get.toDouble) + acc
        )

      val (actualSize, paidShipmentsSize, actualTotal) = ReportingService.yesterdayShipments

      (actualSize, paidShipmentsSize, actualTotal.round) shouldBe (newShipmentsSize, paidShipments.size, expectedTotal.round)
      cleanUpSuccess()
    }
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

      val actualData = ReportingService.yesterdayCancels.map(_.id.get)

      actualData should contain theSameElementsAs expectedIds
      cleanUpSuccess()
    }
  }

  it should "find yesterday sales by agency" in {
    forAll(listOfNPetsChainDataGen(3), listOfNPetsChainDataGen(2), listOfNPetsChainDataGen(2)) {
      (yesterdayNotOursSales, myPetDefenseSales, petlandSales) =>
        val myPetDefenceAgency = createAgency("My Pet Defense")
        val petlandAgency      = createAgency("Petland")
        myPetDefenseSales
          .map(insertUserAndPet)
          .foreach(
            _.user.agency(myPetDefenceAgency).createdAt(anyHourOfYesterday.toDate).saveMe()
          )
        petlandSales
          .map(insertUserAndPet)
          .foreach(_.user.agency(petlandAgency).createdAt(anyHourOfYesterday.toDate).saveMe())

        val someAgencyName = Random.generateString.take(10)
        val someAgency     = createAgency(someAgencyName)

        val insertedPetsSize = yesterdayNotOursSales
          .map(insertUserAndPet)
          .map { inserted =>
            inserted.user.agency(someAgency).createdAt(anyHourOfYesterday.toDate).saveMe()
            inserted.pets.size
          }
          .sum

        val expectedInResult = someAgencyName -> insertedPetsSize

        val actualData = ReportingService.findYesterdaySalesByAgency

        actualData.map(_._1) shouldNot contain("My Pet Defense", "Petland")
        actualData should contain(expectedInResult)
    }
  }

  it should "find mtd shipments" in {
    forAll(genShipmentChainData, genShipmentChainData) { (dataMonth, dataPreviousMonth) =>
      insertUserSubAndShipment(dataPreviousMonth).shipments
        .map(_.createdAt(anyDayUntilThisMonth.toDate).saveMe())
      val expectedShipments = insertUserSubAndShipment(dataMonth).shipments
        .map(_.createdAt(anyDayOfThisMonth.toDate).saveMe().id.get)

      val actualData = ReportingService.findMtdShipments.map(_.id.get)

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

      val actualData = ReportingService.findTodayShipments.map(_.id.get)

      actualData should contain theSameElementsAs expectedShipments
      cleanUpSuccess()
    }
  }

  it should "find current month upcoming subscriptions" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (fromTomorrowData, nextMonthData) =>
        nextMonthData.foreach {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription
              .nextShipDate(anyDayOfNextMonth.toDate)
              .saveMe()
        }
        val expectedSubscriptions = fromTomorrowData.map {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription
              .nextShipDate(anyDayOfThisMonthFromTomorrow.toDate)
              .saveMe()
              .id
              .get
        }

        val actualData = ReportingService.findCurrentMonthUpcomingSubscriptions.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find new today subscriptions" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (todayData, untilTodayData) =>
        untilTodayData.foreach {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription
              .createdAt(anyDayUntilToday.toDate)
              .saveMe()
        }
        val expectedSubscriptions = todayData.map {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription
              .createdAt(anyHourOfToday.toDate)
              .saveMe()
              .id
              .get
        }

        val actualData = ReportingService.findNewTodaySubscriptions.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find new subscriptions for this day in last month" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (thisDayMonthAgoData, anyDayExceptMonthAgoDayData) =>
        anyDayExceptMonthAgoDayData.foreach {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription
              .createdAt(anyDayExceptThisDayMonthAgo.toDate)
              .saveMe()
        }
        val expectedSubscriptions = thisDayMonthAgoData.map {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription
              .createdAt(anyHourOfThisDayMonthAgo.toDate)
              .saveMe()
              .id
              .get
        }

        val actualData = ReportingService.findNewTodaySubscriptionsLastMonth.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find new subscriptions for this day in last year" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (thisDayYearAgoData, anyDayExceptYearAgoDayData) =>
        anyDayExceptYearAgoDayData.foreach {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription
              .createdAt(anyDayExceptThisDayYearAgo.toDate)
              .saveMe()
        }
        val expectedSubscriptions = thisDayYearAgoData.map {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription
              .createdAt(anyHourOfThisDayYearAgo.toDate)
              .saveMe()
              .id
              .get
        }

        val actualData = ReportingService.findNewTodaySubscriptionsLastYear.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find new month subscriptions" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (thisMonthData, anyDayUntilThisMonthData) =>
        anyDayUntilThisMonthData.foreach {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription
              .createdAt(anyDayUntilThisMonth.toDate)
              .saveMe()
        }
        val expectedSubscriptions = thisMonthData.map {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription
              .createdAt(anyDayOfThisMonth.toDate)
              .saveMe()
              .id
              .get
        }

        val actualData = ReportingService.findNewMTDSubscriptions.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find new month subscriptions last month" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (lastMonthData, anyDayUntilLastMonthData) =>
        anyDayUntilLastMonthData.foreach {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription
              .createdAt(anyDayUntilLastMonth.toDate)
              .saveMe()
        }
        val expectedSubscriptions = lastMonthData.map {
          case (uData, sData) =>
            insertUserAndSub(uData, sData).subscription
              .createdAt(anyDayOfLastMonthUntilMonthAgo.toDate)
              .saveMe()
              .id
              .get
        }

        val actualData = ReportingService.findNewMTDSubscriptionsLastMonth.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

}
