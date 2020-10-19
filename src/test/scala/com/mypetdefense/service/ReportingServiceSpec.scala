package com.mypetdefense.service

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers._
import com.mypetdefense.helpers.db.SubscriptionDbUtils._
import com.mypetdefense.helpers.db.UserDbUtils._
import com.mypetdefense.model._
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
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

  it should "find all active subscriptions" in {
    forAll(nonEmptyMapUserNSubscriptionGen, nonEmptyMapUserNSubscriptionGen) {
      (usersWithSub, usersWithoutSub) =>
        val activeSubsIds = usersWithSub.map {
          case (uData, sData) => insertUserAndSub(uData, sData).subscription.id.get
        }
        usersWithoutSub.foreach {
          case (uData, sData) =>
            val u = createUser(uData)
            createSubscription(u, sData).cancel.saveMe()
        }

        val subscriptions   = Subscription.findAll()
        val filteredSubsIds = ReportingService.findActiveSubscriptions(subscriptions).map(_.id.get)

        filteredSubsIds should contain theSameElementsAs activeSubsIds
        clearTables()
        succeed
    }
  }

  it should "find customers for agent" in {
    forAll(nonEmptyUsersGen, nonEmptyUsersGen) { (agentUsers, notAgentUsers) =>
      val agentId       = Random.generateString.take(10)
      val agentUsersIds = agentUsers.map(d => createUser(d).salesAgentId(agentId).saveMe().id.get)
      notAgentUsers.foreach(createUser)

      val customers            = User.findAll()
      val customersForAgent    = ReportingService.findCustomersForAgent(customers, agentId)
      val customersForAgentIds = customersForAgent.map(_.id.get)

      customersForAgent.head.salesAgentId.get shouldBe agentId
      customersForAgentIds should contain theSameElementsAs agentUsersIds
    }
  }

  it should "find new customers for month" in {
    forAll(nonEmptyUsersGen, nonEmptyUsersGen) { (registeredThisMonth, registeredLongTimeAgo) =>
      val expectedUsersIds =
        registeredThisMonth.map(createUser(_).createdAt(anyDayOfThisMonth.toDate).saveMe().id.get)
      registeredLongTimeAgo.foreach(createUser(_).createdAt(anyDayOfLastMonth.toDate).saveMe())

      val users = User.findAll()
      val usersIdsRegisteredInThisMonth =
        ReportingService
          .findNewCustomersMonth(users, now.getMonth.toString, now.getYear)
          .map(_.id.get)

      usersIdsRegisteredInThisMonth should contain theSameElementsAs expectedUsersIds
      clearTables()
      succeed
    }
  }

  it should "find active subscriptions first month" in {
    forAll(
      nonEmptyMapUserNSubscriptionGen,
      nonEmptyMapUserNSubscriptionGen,
      nonEmptyMapUserNSubscriptionGen
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
      clearTables()
      succeed
    }
  }

  it should "find current month subscriptions" in {
    forAll(nonEmptyMapUserNSubscriptionGen, nonEmptyMapUserNSubscriptionGen) {
      (currentMonth, oldOne) =>
        oldOne.foreach {
          case (u, s) =>
            insertUserAndSub(u, s).subscription.createdAt(anyDayOfLastMonth.toDate).saveMe()
        }
        val expectedCurrentMonthIds = currentMonth.map {
          case (u, s) => insertUserAndSub(u, s).subscription.id.get
        }

        val subscriptions = Subscription.findAll()
        val result        = ReportingService.findCurrentMonthSubscriptions(subscriptions).map(_.id.get)

        result should contain theSameElementsAs expectedCurrentMonthIds
        clearTables()
        succeed
    }
  }

  it should "find paid shipments" in {
    forAll(nonEmptyShipmentChainData, nonEmptyShipmentChainData) {
      (dataWithPaidShipments, dataWithUnpaidShipments) =>
        val paidShipmentsIds =
          dataWithPaidShipments
            .map(insertUserSubAndShipment)
            .flatMap(_.shipments.map(_.dateShipped(today).saveMe().id.get))
        dataWithUnpaidShipments
          .map(insertUserSubAndShipment)
          .foreach(_.shipments.foreach(_.taxPaid("0").amountPaid("0").saveMe()))

        val subscriptions = Subscription.findAll()
        val result        = ReportingService.findPaidShipments(subscriptions).map(_.id.get)

        result should contain theSameElementsAs paidShipmentsIds
        clearTables()
        succeed
    }
  }

}
