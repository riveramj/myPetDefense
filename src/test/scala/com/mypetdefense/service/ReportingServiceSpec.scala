package com.mypetdefense.service

import java.util.Date

import bootstrap.liftweb.Boot
import com.mypetdefense.model._
import net.liftweb.common.Empty
import net.liftweb.common.Full
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ReportingServiceSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  override def beforeAll() {
    val boot = new Boot
    boot.boot

    val parent1 = User.createNewUser(
      "Test",
      "User",
      "stripe1234",
      "test@mpd.com",
      "pass",
      "123-123-1234",
      Empty,
      Empty,
      Empty,
      UserType.Parent
    )

    val subscription1 = Subscription.createNewSubscription(
      Full(parent1),
      "stripeSubscription1234",
      new Date(),
      new Date(),
      "defaultPriceCode",
      false,
      0
    )
  }

  override def afterAll() {
    User.findAll().map(_.delete_!)
    Subscription.findAll().map(_.delete_!)
  }

  it should "find all active subscriptions" in {
    val subscriptions = Subscription.findAll()
    ReportingService.findActiveSubscriptions(subscriptions).size should equal (1)
  }
}