package com.mypetdefense.service

import com.mypetdefense.generator.Generator._
import com.mypetdefense.generator._
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers.ListUtil.ListOps
import com.mypetdefense.helpers._
import com.mypetdefense.helpers.db.AgencyDbUtils._
import com.mypetdefense.helpers.db.UserDbUtils._
import com.mypetdefense.model._
import com.mypetdefense.model.domain.reports._
import com.mypetdefense.util.CalculationHelper
import com.mypetdefense.util.RandomIdGenerator.generateLongId
import net.liftweb.common.{Empty, Full}
import net.liftweb.util.Props
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import java.time.LocalDate
import java.util.Date

class ReportingServiceSpec extends DBTest {

  private def createUserAndSubReturnSubId(
      t: (UserCreateGeneratedData, SubscriptionCreateGeneratedData)
  ): Long =
    insertUserAndSub(t._1, t._2).subscription.id.get

  private def createUserAndCancelSubReturnSubId(
      t: (UserCreateGeneratedData, SubscriptionCreateGeneratedData)
  ): Long =
    insertUserAndSub(t._1, t._2).subscription.cancel.saveMe().id.get

  private def insertAgencySalesCreatedAt(
      agency: Agency,
      createdAt: Date,
      data: PetChainData
  ): InsertedUserAndPet = {
    val inserted    = insertUserAndPet(data)
    val updatedUser = inserted.user.referer(agency).createdAt(createdAt).saveMe()
    inserted.copy(user = updatedUser)
  }

  private def insertUpgradeAndCancelPetsAndShipmentData(
      data: List[PetsAndShipmentChainData],
      agency: Agency
  ): List[InsertedPetsUserSubAndShipment] =
    data
      .map(insertPetAndShipmentsChainAtAgency(_, agency, subUpgraded = true))
      .map(cancelSubscription)

  private def cancelSubscription(in: InsertedPetsUserSubAndShipment) =
    in.copy(subscription = in.subscription.cancel)

  private def upgradedSubsByAgency(
      input: List[InsertedPetsUserSubAndShipment]
  ): Iterable[CountedByAgency] = {
    val agenciesNames = input.flatMap(_.user.referer.toList).map { a =>
      a.name.get match {
        case "Some agency1" => mpdAgencyName
        case "Some agency2" => tppAgencyName
        case x              => x
      }
    }
    CalculationHelper
      .calculateOccurrences[String, String](agenciesNames, identity)
      .map(CountedByAgency.tupled)
  }

  private def calculateOccurrencesSubsByShipments(
      input: List[InsertedPetsUserSubAndShipment]
  ): Iterable[CancelledUpgradedSubscriptionsByCount] =
    CalculationHelper
      .calculateOccurrences[Int, InsertedPetsUserSubAndShipment](input, _.shipments.size)
      .map(CancelledUpgradedSubscriptionsByCount.tupled)

  private def calculateOccurrencesPetsByCount(
      input: List[InsertedPetsUserSubAndShipment]
  ): Iterable[CancelledUpgradedSubscriptionsByCount] =
    CalculationHelper
      .calculateOccurrences[Int, Int](
        input.map(_.pets.size),
        identity
      )
      .map(CancelledUpgradedSubscriptionsByCount.tupled)

  private def calculateOccurrencesPetsByProduct(
      input: List[InsertedPetsUserSubAndShipment]
  ): Iterable[PetsByProduct] = {
    val fleaTick = input.flatMap(_.subscription.subscriptionBoxes).flatMap(_.fleaTick.toList)
    CalculationHelper
      .calculateOccurrences[String, FleaTick](fleaTick, _.getNameAndSize)
      .map(PetsByProduct.tupled)
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
      val oldIds =
        registeredLongTimeAgo.map(createUser(_).createdAt(anyDayOfLastMonth.toDate).saveMe().id.get)

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
        val cancelledIds =
          anyDayOfThisYearUntilThisMonth.fold(Iterable.empty[Long]) { anyDayUntilThisMonth =>
            cancelledInCurrentYear.map {
              case (uData, sData) =>
                insertUserAndSub(uData, sData).subscription.cancel
                  .cancellationDate(anyDayUntilThisMonth.toDate)
                  .saveMe()
                  .id
                  .get
            }
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
        paidShipments.foldLeft(BigDecimal(0d))((acc, s) =>
          (BigDecimal(s.amountPaid.get) - BigDecimal(s.taxPaid.get)) + acc
        )

      val (actualSize, paidShipmentsSize, actualTotal) = ReportingService.yesterdayShipments

      (actualSize, paidShipmentsSize, actualTotal) shouldBe (newShipmentsSize, paidShipments.size, expectedTotal)
      cleanUpSuccess()
    }
  }

  it should "find yesterday sales by agency" in {
    forAll(listOfNPetsChainDataGen(3), listOfNPetsChainDataGen(2), listOfNPetsChainDataGen(2)) {
      (yesterdayNotOursSales, myPetDefenseSales, petlandSales) =>
        val mpdAndPetland = createPetlandAndMPDAgencies()
        myPetDefenseSales.foreach(
          insertAgencySalesCreatedAt(mpdAndPetland.mpd, anyHourOfYesterday.toDate, _)
        )
        petlandSales.foreach(
          insertAgencySalesCreatedAt(mpdAndPetland.petland, anyHourOfYesterday.toDate, _)
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

  it should "find MTD sales by agency" in {
    forAll(listOfNPetsChainDataGen(3), listOfNPetsChainDataGen(2), listOfNPetsChainDataGen(2)) {
      (yesterdayMonthNotOursSales, myPetDefenseSales, petlandSales) =>
        val mpdAndPetland = createPetlandAndMPDAgencies()
        myPetDefenseSales.foreach(
          insertAgencySalesCreatedAt(mpdAndPetland.mpd, anyDayOfThisMonth.toDate, _)
        )
        petlandSales.foreach(
          insertAgencySalesCreatedAt(mpdAndPetland.petland, anyDayOfThisMonth.toDate, _)
        )

        val someAgencyName = Random.generateString.take(10)
        val someAgency     = createAgency(someAgencyName)

        val insertedPetsSize = yesterdayMonthNotOursSales
          .map(insertAgencySalesCreatedAt(someAgency, anyDayOfThisMonth.toDate, _).pets.size)
          .sum

        val expectedInResult = someAgencyName -> insertedPetsSize

        val actualData = ReportingService.findYesterdayMTDSalesByAgency

        actualData.map(_._1) shouldNot contain("My Pet Defense", "Petland")
        actualData should contain(expectedInResult)
    }
  }

  it should "find yesterday sales by agent" in {
    forAll(listOfNPetsChainDataGen(3), listOfNPetsChainDataGen(2), listOfNPetsChainDataGen(2)) {
      (yesterdayNotOursSales, myPetDefenseSales, petlandSales) =>
        val mpdAndPetland = createPetlandAndMPDAgencies()
        myPetDefenseSales.foreach(
          insertAgencySalesCreatedAt(mpdAndPetland.mpd, anyHourOfYesterday.toDate, _)
        )
        petlandSales.foreach(
          insertAgencySalesCreatedAt(mpdAndPetland.petland, anyHourOfYesterday.toDate, _)
        )

        val someAgencyName   = Random.generateString.take(10)
        val someSalesAgentId = Random.generateString.take(10)
        val someAgency       = createAgency(someAgencyName).saveMe()

        val insertedPetsSize = yesterdayNotOursSales
          .map(insertAgencySalesCreatedAt(someAgency, anyHourOfYesterday.toDate, _))
          .map { inserted =>
            inserted.user.salesAgentId(someSalesAgentId).saveMe()
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
        val mpdAndPetland = createPetlandAndMPDAgencies()
        myPetDefenseSales.foreach(
          insertAgencySalesCreatedAt(mpdAndPetland.mpd, anyDayOfThisMonth.toDate, _)
        )
        petlandSales.foreach(
          insertAgencySalesCreatedAt(
            createPetlandAndMPDAgencies().petland,
            anyDayOfThisMonth.toDate,
            _
          )
        )

        val someAgencyName   = Random.generateString.take(10)
        val someSalesAgentId = Random.generateString.take(10)
        val someAgency       = createAgency(someAgencyName).saveMe()

        val insertedPetsSize = yesterdayMonthNotOursSales
          .map(insertAgencySalesCreatedAt(someAgency, anyDayOfThisMonth.toDate, _))
          .map { inserted =>
            inserted.user.salesAgentId(someSalesAgentId).saveMe()
            inserted.pets.size
          }
          .sum

        val expectedInResult = someSalesAgentId -> insertedPetsSize

        val actualData = ReportingService.findYesterdayMTDSalesByAgent

        actualData.map(_._1) shouldNot contain("My Pet Defense", "Petland")
        actualData should contain(expectedInResult)
    }
  }

  it should "find cancels by shipment" in {
    forAll(listOfNShipmentChainDataGen(), genShipmentChainData, genShipmentChainData) {
      (shouldBeInStatisticData, notCanceledData, canceledAndNotShippedData) =>
        insertUserSubAndShipment(notCanceledData)
        insertUserSubAndShipment(canceledAndNotShippedData).subscription.cancel

        val canceledShouldBeInResultSize =
          shouldBeInStatisticData.map(insertUserSubAndShipment).map { inserted =>
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

  it should "form a proper executive snapshot report" in {
    forAll(
      listOfNPetsAndShipmentChainDataGen(6),
      listOfNPetsAndShipmentChainDataGen(4),
      listOfNPetsAndShipmentChainDataGen(2),
      listOfNPetsAndShipmentChainDataGen(2)
    ) { (upgradeActiveData, upgradedAndCancelledData, otherUpgradedData, otherNotUpgradedData) =>
      val myPetDefenseAgency                                 = createAgency(mpdAgencyName)
      val tppAgency                                          = createAgency(tppAgencyName)
      val someAgency1                                        = createAgency("Some agency1").parent(myPetDefenseAgency).saveMe()
      val someAgency2                                        = createAgency("Some agency2").parent(tppAgency).saveMe()
      val (otherActiveUpdatedData1, otherActiveUpdatedData2) = otherUpgradedData.splitInTwo
      val (upgradedMPD, upgradedTPP)                         = upgradeActiveData.splitInTwo
      val (upgradedCancelledMPD, upgradedCancelledTPP)       = upgradedAndCancelledData.splitInTwo

      val insertedCldUpgMpd =
        insertUpgradeAndCancelPetsAndShipmentData(upgradedCancelledMPD, myPetDefenseAgency)
      val insertedCldUpgTPP =
        insertUpgradeAndCancelPetsAndShipmentData(upgradedCancelledTPP, tppAgency)
      val insertedUpgMPD =
        insertPetsAndShipmentData(upgradedMPD, myPetDefenseAgency, subUpgraded = true)
      val insertedUpgTPP = insertPetsAndShipmentData(upgradedTPP, tppAgency, subUpgraded = true)
      val insertedUpgActiveOtherData1 =
        insertPetsAndShipmentData(otherActiveUpdatedData1, someAgency1, subUpgraded = true)
      val insertedUpgActiveOtherData2 =
        insertPetsAndShipmentData(otherActiveUpdatedData2, someAgency2, subUpgraded = true)

      val insertedNotUpgradedActiveData =
        insertPetsAndShipmentData(otherNotUpgradedData, tppAgency, subUpgraded = false)

      val allActiveData = List(
        insertedUpgMPD,
        insertedUpgTPP,
        insertedUpgActiveOtherData1,
        insertedUpgActiveOtherData2,
        insertedNotUpgradedActiveData
      ).flatten
      val allActiveUpgradedData = List(
        insertedUpgMPD,
        insertedUpgTPP,
        insertedUpgActiveOtherData1,
        insertedUpgActiveOtherData2
      ).flatten
      val allCancelledUpgradedData = List(insertedCldUpgMpd, insertedCldUpgTPP).flatten

      val allPetsSize               = allActiveData.map(_.pets.size).sum
      val allActiveUpgradedPetsSize = allActiveUpgradedData.map(_.pets.size).sum

      val expectedAllAccountsReport = AllAccountsReport(allActiveData.size, allPetsSize)
      val expectedUpgradedSubscriptionsReport = UpgradedSubscriptionsReport(
        allActiveUpgradedData.size,
        allActiveUpgradedPetsSize,
        allCancelledUpgradedData.size
      )
      val expectedActiveUpdPetsBySize = calculateOccurrencesPetsByProduct(allActiveUpgradedData)
      val expectedCancelledUpdPetsBySize =
        calculateOccurrencesPetsByProduct(allCancelledUpgradedData)
      val expectedCancelledUpgradedSubsByPetCount =
        calculateOccurrencesPetsByCount(allCancelledUpgradedData)
      val expectedCancelledUpgradedSubsByShipmentCount =
        calculateOccurrencesSubsByShipments(allCancelledUpgradedData)
      val expectedActiveUpgradesByAgency    = upgradedSubsByAgency(allActiveUpgradedData)
      val expectedCancelledUpgradesByAgency = upgradedSubsByAgency(allCancelledUpgradedData)

      val actualData = ReportingService.executiveSnapshotReport

      actualData.allAccountsReport shouldBe expectedAllAccountsReport
      actualData.upgradedSubscriptionsReport shouldBe expectedUpgradedSubscriptionsReport
      actualData.activeUpgradedPetsByProduct should contain theSameElementsAs expectedActiveUpdPetsBySize
      actualData.cancelledUpgradedPetsByProduct should contain theSameElementsAs expectedCancelledUpdPetsBySize
      actualData.cancelledUpgradedSubsByPetCount should contain theSameElementsAs expectedCancelledUpgradedSubsByPetCount
      actualData.cancelledUpgradedSubsByShipmentCount should contain theSameElementsAs expectedCancelledUpgradedSubsByShipmentCount
      actualData.activeUpgradesByAgency should contain theSameElementsAs expectedActiveUpgradesByAgency
      actualData.cancelledUpgradesByAgency should contain theSameElementsAs expectedCancelledUpgradesByAgency

      cleanUpSuccess()
    }
  }

  it should "count shipments in period" in {
    val subs = List(
      makeUserSubAndNShipments(LocalDate.of(2020, 10, 5), 3),
      makeUserSubAndNShipments(LocalDate.of(2020, 10, 5), 2),
      makeUserSubAndNShipments(LocalDate.of(2020, 10, 5), 1)
    )

    val period = RetentionPeriod(10, 2020)

    ReportingService.shipmentCountsForPeriod(subs, period, shipmentsCount = 3) mustBe List(3, 2, 1)
  }

  it should "select proper periods and create a retention report" in {
    makeUserSubAndNShipments(LocalDate.of(2020, 9, 5), 4)
    makeUserSubAndNShipments(LocalDate.of(2020, 9, 5), 4)
    makeUserSubAndNShipments(LocalDate.of(2020, 9, 5), 3)
    makeUserSubAndNShipments(LocalDate.of(2020, 9, 5), 2)
    makeUserSubAndNShipments(LocalDate.of(2020, 10, 5), 3)
    makeUserSubAndNShipments(LocalDate.of(2020, 10, 5), 3)
    makeUserSubAndNShipments(LocalDate.of(2020, 10, 5), 2)
    makeUserSubAndNShipments(LocalDate.of(2020, 11, 5), 2)
    makeUserSubAndNShipments(LocalDate.of(2020, 11, 5), 2)
    makeUserSubAndNShipments(LocalDate.of(2020, 12, 5), 1)
    makeUserSubAndNShipments(LocalDate.of(2020, 12, 5), 1)
    makeUserSubAndNShipments(LocalDate.of(2020, 12, 5), 1, "tpp")

    val lastPeriod = RetentionPeriod(12, 2020)

    val subSize = 4 + 4 + 3 + 2 + 3 + 3 + 2 + 2 + 2 + 1 + 1

    ReportingService.subscriptionRetentionReport(lastPeriod, periodsCount = 3) mustBe
      SubscriptionRetentionReport(
        List(
          SubscriptionRetentionForPeriod(RetentionPeriod(10, 2020), subSize, List(3, 3, 2)),
          SubscriptionRetentionForPeriod(RetentionPeriod(11, 2020), subSize, List(2, 2)),
          SubscriptionRetentionForPeriod(RetentionPeriod(12, 2020), subSize, List(2))
        )
      )
  }

  private def makeUserSubAndNShipments(startDate: LocalDate, n: Int, priceCode: String = "default"): Subscription = {
    val user = User.createNewUser(
      firstName = "John",
      lastName = "Doe",
      stripeId = "cus_1234",
      email = "john@example.com",
      password = "1234",
      phone = "123456789",
      coupon = Empty,
      referer = Empty,
      agency = Empty,
      UserType.Agent,
      ""
    )

    val start = startDate.atStartOfDay(zoneId)
    val hwPriceCode =
      if (priceCode == "default")
        Props.get("default.price.code").openOr("")
      else
        priceCode

    val sub = Subscription.createNewSubscription(
      Full(user),
      stripeSubscriptionId = "sub_1234",
      priceCode = hwPriceCode,
      startDate = Date.from(start.toInstant),
      nextShipDate = Date.from(start.plusDays(5).toInstant)
    )

    (0 until n).reverse foreach { i =>
      val dateProcessed    = start.minusMonths(i)
      val expectedShipDate = dateProcessed.plusDays(5)

      Shipment.create
        .shipmentId(generateLongId)
        .stripePaymentId("pay_1234")
        .stripeChargeId("")
        .subscription(sub)
        .expectedShipDate(Date.from(expectedShipDate.toInstant))
        .dateProcessed(Date.from(dateProcessed.toInstant))
        .amountPaid("10.00")
        .taxPaid("2.00")
        .shipmentStatus(ShipmentStatus.Paid)
        .freeUpgradeSample(false)
        .saveMe
    }

    sub.reload
  }

}
