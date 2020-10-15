package com.mypetdefense.service

import com.mypetdefense.helpers.DBTestUtil._
import com.mypetdefense.helpers.{BootUtil, DBTestUtil}
import com.mypetdefense.model._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ReportingServiceSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  override def beforeAll() {
    BootUtil.bootForTests()
  }

  override def afterAll(): Unit = {
    DBTestUtil.clearTables()
  }

  it should "find all active subscriptions" in {
    val activeSubscriptionUser = createUser()
    createSubscription(activeSubscriptionUser)

    val inactiveSubscriptionUser = createUser()
    createSubscription(inactiveSubscriptionUser).cancel

    val subscriptions = Subscription.findAll()
    ReportingService.findActiveSubscriptions(subscriptions).size shouldBe 1
  }
}
