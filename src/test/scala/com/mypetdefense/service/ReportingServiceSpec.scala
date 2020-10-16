package com.mypetdefense.service

import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers._
import com.mypetdefense.helpers.db.SubscriptionDbUtils._
import com.mypetdefense.helpers.db.UserDbUtils._
import com.mypetdefense.model._
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ReportingServiceSpec
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach
    with BeforeAndAfterAll {
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
    val activeSubscriptionUser = createUser()
    createSubscription(activeSubscriptionUser)

    val inactiveSubscriptionUser = createUser()
    createSubscription(inactiveSubscriptionUser).cancel

    val subscriptions = Subscription.findAll()
    ReportingService.findActiveSubscriptions(subscriptions).size shouldBe 1
  }

  it should "find customers for agent" in {
    val agentId = Random.generateString
    createUser()
    createUser()
    val userForAgent = createUser().salesAgentId(agentId).saveMe()

    val customers         = User.findAll()
    val customersForAgent = ReportingService.findCustomersForAgent(customers, agentId)
    customersForAgent.size shouldBe 1
    customersForAgent.map(_.id.get) should contain(userForAgent.id.get)
  }

}
