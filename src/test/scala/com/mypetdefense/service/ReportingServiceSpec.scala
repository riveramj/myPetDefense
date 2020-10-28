package com.mypetdefense.service

import java.util.Date
import java.time.ZoneId

import com.mypetdefense.generator.Generator.{listOfNShipmentChainData, _}
import com.mypetdefense.generator.{
  PetChainData,
  SubscriptionCreateGeneratedData,
  UserCreateGeneratedData
}
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers._
import com.mypetdefense.helpers.db.UserDbUtils._
import com.mypetdefense.helpers.db.AgencyDbUtils._
import com.mypetdefense.helpers.models.PetlandAndMPDAgencies
import com.mypetdefense.model._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ReportingServiceSpec extends DBTest {

  private val mpdAgencyName     = "My Pet Defense"
  private val petLandAgencyName = "Petland"

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

  private def createPetlandAndMPDAgencies(): PetlandAndMPDAgencies = {
    val myPetDefenseAgency = createAgency(mpdAgencyName)
    val petlandAgency      = createAgency(petLandAgencyName)
    PetlandAndMPDAgencies(petlandAgency, myPetDefenseAgency)
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

        val actualData = ReportingService.findMTDSalesByAgency

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

        val actualData = ReportingService.findMTDSalesByAgent

        actualData.map(_._1) shouldNot contain("My Pet Defense", "Petland")
        actualData should contain(expectedInResult)
    }
  }

  it should "find cancels by shipment" in {
    forAll(listOfNShipmentChainData(), genShipmentChainData, genShipmentChainData) {
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

  it should "form a proper quickhit report" in {
    forAll(listOfNShipmentChainData(), listOfNShipmentChainData()) { (data1, data2) =>
      val agency = createPetlandAndMPDAgencies()
      data1.map(insertUserSubAndShipment)
      data2.map(insertUserSubAndShipment).foreach(_.subscription.cancel)

      val x = ReportingService.getQuickHitReport

      succeed
    }
  }

}
