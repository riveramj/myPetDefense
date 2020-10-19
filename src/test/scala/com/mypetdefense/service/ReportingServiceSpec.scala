package com.mypetdefense.service

import com.mypetdefense.generator.Generator._
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
          case (uData, sData) =>
            val u = createUser(uData)
            createSubscription(u, sData).id.get
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

}
