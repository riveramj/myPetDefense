package com.mypetdefense.service

import java.util.Date
import java.time.ZoneId
import com.mypetdefense.generator.Generator._
import com.mypetdefense.generator.{
  PetChainData,
  SubscriptionCreateGeneratedData,
  UserCreateGeneratedData
}
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers.Random.randomPosLong
import com.mypetdefense.helpers._
import com.mypetdefense.helpers.db.UserDbUtils._
import com.mypetdefense.helpers.db.AgencyDbUtils._
import com.mypetdefense.model._
import org.scalatest.{Assertion, BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.immutable

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

  private def insertAgencySalesCreatedAt(
      agency: Agency,
      createdAt: Date,
      data: PetChainData
  ): InsertedUserAndPet = {
    val inserted    = insertUserAndPet(data)
    val updatedUser = inserted.user.referer(agency).createdAt(createdAt).saveMe()
    inserted.copy(user = updatedUser)
  }

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
        myPetDefenseSales.foreach(
          insertAgencySalesCreatedAt(myPetDefenceAgency, anyHourOfYesterday.toDate, _)
        )
        petlandSales.foreach(
          insertAgencySalesCreatedAt(petlandAgency, anyHourOfYesterday.toDate, _)
        )

        val someAgencyName = Random.generateString.take(10)
        val someAgency     = createAgency(someAgencyName)

        val insertedPetsSize = yesterdayNotOursSales
          .map(insertAgencySalesCreatedAt(someAgency, anyHourOfYesterday.toDate, _).pets.size)
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
        val expectedSubscriptions = insertSubscriptionsForTests(
          fromTomorrowData,
          nextMonthData,
          _.nextShipDate(anyDayOfThisMonthFromTomorrow.toDate).saveMe(),
          _.nextShipDate(anyDayOfNextMonth.toDate).saveMe()
        ).map(_.id.get)

        val actualData = ReportingService.findCurrentMonthUpcomingSubscriptions.map(_.id.get)

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
          _.createdAt(anyHourOfToday.toDate).saveMe(),
          _.createdAt(anyDayUntilToday.toDate).saveMe()
        ).map(_.id.get)

        val actualData = ReportingService.findNewTodaySubscriptions.map(_.id.get)

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
          _.createdAt(anyHourOfThisDayMonthAgo.toDate).saveMe(),
          _.createdAt(anyDayExceptThisDayMonthAgo.toDate).saveMe()
        ).map(_.id.get)

        val actualData = ReportingService.findNewTodaySubscriptionsLastMonth.map(_.id.get)

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
          _.createdAt(anyHourOfThisDayYearAgo.toDate).saveMe(),
          _.createdAt(anyDayExceptThisDayYearAgo.toDate).saveMe()
        ).map(_.id.get)

        val actualData = ReportingService.findNewTodaySubscriptionsLastYear.map(_.id.get)

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
          _.createdAt(anyDayOfThisMonth.toDate).saveMe(),
          _.createdAt(anyDayUntilThisMonth.toDate).saveMe()
        ).map(_.id.get)

        val actualData = ReportingService.findNewMTDSubscriptions.map(_.id.get)

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
          _.createdAt(anyDayOfLastMonthUntilMonthEnd.toDate).saveMe(),
          _.createdAt(anyDayUntilLastMonth.toDate).saveMe()
        ).map(_.id.get)

        val actualData = ReportingService.findNewMTDSubscriptionsLastMonth.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find new month subscriptions last year" in {
    forAll(mapWithNOfUserNSubscriptionGen(), mapWithNOfUserNSubscriptionGen()) {
      (thisMonthLastYearData, anyDayFromThisDayYearAgoData) =>
        val expectedSubscriptions = insertSubscriptionsForTests(
          thisMonthLastYearData,
          anyDayFromThisDayYearAgoData,
          _.createdAt(anyDayOfLastYearThisDay.toDate).saveMe(),
          _.createdAt(anyDayOfLastYearFromThisDayYearAgo.toDate).saveMe()
        ).map(_.id.get)

        val actualData = ReportingService.findNewMTDSubscriptionsLastYear.map(_.id.get)

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
          _.createdAt(anyDayOfThisYear.toDate).saveMe(),
          _.createdAt(anyDayOfLastYear.toDate).saveMe()
        ).map(_.id.get)

        val actualData = ReportingService.findNewYTDSubscriptions.map(_.id.get)

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
          _.createdAt(anyDayOfThisYearUntilMonthAgo.toDate).saveMe(),
          _.createdAt(anyDayOfThisYearFromMonthAgo.toDate).saveMe()
        ).map(_.id.get)

        val actualData = ReportingService.findNewYTDSubscriptionsLastMonth.map(_.id.get)

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
          _.createdAt(anyDayOfLastYearThisDay.toDate).saveMe(),
          _.createdAt(anyDayOfLastYearFromThisDayYearAgo.toDate).saveMe()
        ).map(_.id.get)

        val actualData = ReportingService.findNewYTDSubscriptionsLastYear.map(_.id.get)

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

        val actualData = ReportingService.findCancelledMtdSubscriptions.map(_.id.get)

        actualData should contain theSameElementsAs expectedSubscriptions
        cleanUpSuccess()
    }
  }

  it should "find MTD sales by agency" in {
    forAll(listOfNPetsChainDataGen(3), listOfNPetsChainDataGen(2), listOfNPetsChainDataGen(2)) {
      (yesterdayMonthNotOursSales, myPetDefenseSales, petlandSales) =>
        val myPetDefenceAgency = createAgency("My Pet Defense")
        val petlandAgency      = createAgency("Petland")
        myPetDefenseSales
          .map(insertUserAndPet)
          .foreach(
            _.user.referer(myPetDefenceAgency).createdAt(anyDayOfYesterdayMonth.toDate).saveMe()
          )
        petlandSales
          .map(insertUserAndPet)
          .foreach(_.user.referer(petlandAgency).createdAt(anyDayOfYesterdayMonth.toDate).saveMe())

        val someAgencyName = Random.generateString.take(10)
        val someAgency     = createAgency(someAgencyName)

        val insertedPetsSize = yesterdayMonthNotOursSales
          .map(insertUserAndPet)
          .map { inserted =>
            inserted.user.referer(someAgency).createdAt(anyDayOfYesterdayMonth.toDate).saveMe()
            inserted.pets.size
          }
          .sum

        val expectedInResult = someAgencyName -> insertedPetsSize

        val actualData = ReportingService.findMTDSalesByAgency

        actualData.map(_._1) shouldNot contain("My Pet Defense", "Petland")
        actualData should contain(expectedInResult)
    }
  }

  it should "find yesterday sales by agent" in {
    forAll(listOfNPetsChainDataGen(3), listOfNPetsChainDataGen(2), listOfNPetsChainDataGen(2)) {
      (yesterdayNotOursSales, myPetDefenseSales, petlandSales) =>
        val myPetDefenceAgency = createAgency("My Pet Defense").agencyId(randomPosLong).saveMe()
        val petlandAgency      = createAgency("Petland").agencyId(randomPosLong).saveMe()
        myPetDefenseSales
          .map(insertUserAndPet)
          .foreach(
            _.user.referer(myPetDefenceAgency).createdAt(anyHourOfYesterday.toDate).saveMe()
          )
        petlandSales
          .map(insertUserAndPet)
          .foreach(_.user.referer(petlandAgency).createdAt(anyHourOfYesterday.toDate).saveMe())

        val someAgencyName   = Random.generateString.take(10)
        val someSalesAgentId = Random.generateString.take(10)
        val someAgency       = createAgency(someAgencyName).saveMe()

        val insertedPetsSize = yesterdayNotOursSales
          .map(insertUserAndPet)
          .map { inserted =>
            inserted.user
              .referer(someAgency)
              .salesAgentId(someSalesAgentId)
              .createdAt(anyHourOfYesterday.toDate)
              .saveMe()
            inserted.pets.size
          }
          .sum

        val expectedInResult = someSalesAgentId -> insertedPetsSize

        val actualData = ReportingService.findYesterdaySalesByAgent

        actualData.map(_._1) shouldNot contain("My Pet Defense", "Petland")
        actualData should contain(expectedInResult)
    }
  }

  it should "find MTD sales by agent" in {
    forAll(listOfNPetsChainDataGen(3), listOfNPetsChainDataGen(2), listOfNPetsChainDataGen(2)) {
      (yesterdayMonthNotOursSales, myPetDefenseSales, petlandSales) =>
        val myPetDefenceAgency = createAgency("My Pet Defense").agencyId(randomPosLong).saveMe()
        val petlandAgency      = createAgency("Petland").agencyId(randomPosLong).saveMe()
        myPetDefenseSales
          .map(insertUserAndPet)
          .foreach(
            _.user.referer(myPetDefenceAgency).createdAt(anyDayOfYesterdayMonth.toDate).saveMe()
          )
        petlandSales
          .map(insertUserAndPet)
          .foreach(_.user.referer(petlandAgency).createdAt(anyDayOfYesterdayMonth.toDate).saveMe())

        val someAgencyName   = Random.generateString.take(10)
        val someSalesAgentId = Random.generateString.take(10)
        val someAgency       = createAgency(someAgencyName).saveMe()

        val insertedPetsSize = yesterdayMonthNotOursSales
          .map(insertUserAndPet)
          .map { inserted =>
            inserted.user
              .referer(someAgency)
              .salesAgentId(someSalesAgentId)
              .createdAt(anyDayOfYesterdayMonth.toDate)
              .saveMe()
            inserted.pets.size
          }
          .sum

        val expectedInResult = someSalesAgentId -> insertedPetsSize

        val actualData = ReportingService.findMTDSalesByAgent

        actualData.map(_._1) shouldNot contain("My Pet Defense", "Petland")
        actualData should contain(expectedInResult)
    }
  }

  it should "find cancels by shipment" in {
    forAll(listOfNShipmentChainData(), genShipmentChainData, genShipmentChainData) {
      (shouldBeInStatData, notCanceledData, canceledAndNotShippedData) =>
        insertUserSubAndShipment(notCanceledData)
        insertUserSubAndShipment(canceledAndNotShippedData).subscription.cancel

        val canceledShouldBeInResultSize = shouldBeInStatData.map(insertUserSubAndShipment).map {
          inserted =>
            inserted.subscription.cancel
            inserted.shipments.map(_.dateShipped(anyDayOfThisYear.toDate).saveMe()).size
        }

        val expectedData = ("0", 1) :: List(1, 2, 3, 4, 5).map { count =>
          (count.toString, canceledShouldBeInResultSize.count(_ == count))
        } ++ List(("6+", canceledShouldBeInResultSize.count(_ >= 6)))

        val subs = Subscription.findAll()

        val actualData = ReportingService.cancelsByShipment(subs)

        actualData should contain theSameElementsAs expectedData
        cleanUpSuccess()
    }
  }

  it should "find same day cancels by month" in {
    forAll(listOfNShipmentChainData(), genShipmentChainData, genShipmentChainData) {
      (shouldBeInStatData, notCanceledData, canceledAndShippedData) =>
        insertUserSubAndShipment(notCanceledData)
        val shouldBeCanceledAndHaveShipping = insertUserSubAndShipment(canceledAndShippedData)
        shouldBeCanceledAndHaveShipping.subscription.cancel
        shouldBeCanceledAndHaveShipping.shipments.map(
          _.dateShipped(anyDayOfThisYear.toDate).saveMe()
        )

        val expectedData = shouldBeInStatData
          .map(insertUserSubAndShipment)
          .map { inserted =>
            inserted.subscription.cancel
            inserted.subscription.createdAt(anyDayOfThisYear.toDate).saveMe()
          }
          .groupBy(_.createdAt.get.toInstant.atZone(ZoneId.systemDefault()).toLocalDate.getMonth)
          .mapValues(_.size)

        val subs = Subscription.findAll()

        val actualData = ReportingService.sameDayCancelsByMonth(subs)

        actualData should contain theSameElementsAs expectedData
        cleanUpSuccess()
    }
  }

}
