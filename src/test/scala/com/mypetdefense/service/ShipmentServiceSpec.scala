package com.mypetdefense.service

import java.util.Date
import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers.Random._
import com.mypetdefense.helpers.TestTags.PostgresOnlyTest
import com.mypetdefense.helpers.db.AgencyDbUtils.createAgency
import com.mypetdefense.model._
import net.liftweb.common.Full
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ShipmentServiceSpec extends DBTest {
  import ShipmentServiceSpec._

  it should "create new shipment" in {
    forAll(userAndSubscriptionGen) {
      case (userInsertData, subInsertData) =>
        val inserted   = insertUserAndSub(userInsertData, subInsertData)
        val invoiceId  = generateString.take(10)
        val chargeId   = Full(generateString.take(10))
        val amountPaid = generateMoneyString
        val tax        = generateMoneyString

        ShipmentService.createNewShipment(
          inserted.user,
          invoiceId,
          chargeId,
          amountPaid,
          tax
        )
        val maybeCreatedShipment = Shipment.findAll().headOption
        maybeCreatedShipment.fold(fail("The shipment wasn't created")) { shipment =>
          shipment.stripePaymentId.get shouldBe invoiceId
          shipment.stripeChargeId.get shouldBe chargeId.toOption.get
          shipment.amountPaid.get shouldBe amountPaid
          shipment.taxPaid.get shouldBe tax
        }
        cleanUpSuccess()
    }
  }

  it should "get current past due shipments" taggedAs PostgresOnlyTest in {
    val properDate            = validDateForGetCurrentPastDueShipments
    val shouldBeProperData    = listOfNShipmentChainData()
    val shouldBeCancelledData = listOfNShipmentChainData()
    val shouldBeUnpaidData    = listOfNShipmentChainData()
    val shouldBeTooFutureData = listOfNShipmentChainData()
    val insertedProperDataIds = shouldBeProperData
      .map(insertUserSubAndShipment)
      .flatMap(
        setExpShipDateStatusAndShipmentStatus(
          _,
          properDate.toDate,
          ShipmentStatus.Paid,
          Status.Active
        )
      )
      .map(_.id.get)
    shouldBeCancelledData
      .map(insertUserSubAndShipment)
      .flatMap(
        setExpShipDateStatusAndShipmentStatus(
          _,
          properDate.toDate,
          ShipmentStatus.Paid,
          Status.Cancelled
        )
      )
    shouldBeUnpaidData
      .map(insertUserSubAndShipment)
      .flatMap(
        setExpShipDateStatusAndShipmentStatus(
          _,
          properDate.toDate,
          ShipmentStatus.Other,
          Status.Active
        )
      )

    shouldBeTooFutureData
      .map(insertUserSubAndShipment)
      .flatMap(
        setExpShipDateStatusAndShipmentStatus(
          _,
          anyDayOfThisMonthFromTomorrow.toDate,
          ShipmentStatus.Paid,
          Status.Active
        )
      )

    val actualDataIds = ShipmentService.getCurrentPastDueShipments.map(_.id.get)

    actualDataIds should contain theSameElementsAs insertedProperDataIds
  }

  it should "get upcoming subscriptions" taggedAs PostgresOnlyTest in {
    val shouldBeProperData    = mapWithNOfUserNSubscription()
    val shouldBeCancelledData = mapWithNOfUserNSubscription()
    val shouldBeTooFutureData = mapWithNOfUserNSubscription()
    val insertedExpectedIds = shouldBeProperData
      .map(insertUserAndSubTupled)
      .map(setStatusAndNextShipDate(_, anyDayOfNext19Days.toDate, Status.Active))
      .map(_.id.get)
    shouldBeCancelledData
      .map(insertUserAndSubTupled)
      .map(setStatusAndNextShipDate(_, anyDayOfNext19Days.toDate, Status.Cancelled))
    shouldBeTooFutureData
      .map(insertUserAndSubTupled)
      .map(setStatusAndNextShipDate(_, anyDayOFromPlus21Days.toDate, Status.Active))
    val actualDataIds = ShipmentService.getUpcomingSubscriptions.map(_.id.get)

    actualDataIds should contain theSameElementsAs insertedExpectedIds
  }

  it should "get past due shipments" taggedAs PostgresOnlyTest in {
    val shouldBeProperData    = mapWithNOfUserNSubscription()
    val shouldBeTooFutureData = mapWithNOfUserNSubscription()
    val insertedExpectedIds = shouldBeProperData
      .map(insertUserAndSubTupled)
      .map(setStatusAndNextShipDate(_, validDateForGetPastDueShipments.toDate, Status.Active))
      .map(_.id.get)
    shouldBeTooFutureData
      .map(insertUserAndSubTupled)
      .map(setStatusAndNextShipDate(_, invalidDateForGetPastDueShipments.toDate, Status.Active))
      .map(_.id.get)
    val actualDataIds = ShipmentService.getPastDueShipments.map(_.id.get)

    actualDataIds should contain theSameElementsAs insertedExpectedIds
  }

  "shouldSendFreeUpgradeShipment" should "send upgrade on second shipment for TPP" in {
    forAllNoShrink(listOfNSimplePetsGen()) { pets =>
      val dogs = pets.filter(_.animalType.get == AnimalType.Dog)
      whenever(dogs.nonEmpty) {
        val sub = subscriptionWithFreeUpgrade.saveMe()
        subscriptionBoxWithFreeUpgrade(sub).saveMe()

        ShipmentService.shouldSendFreeUpgradeShipment(sub, shipmentCount = 1, dogs) mustBe true

        clearTables()
      }
    }
  }

  it should "not send upgrade on second shipment for non-tpp" in {
    forAllNoShrink(listOfNSimplePetsGen()) { pets =>
      val dogs = pets.filter(_.animalType.get == AnimalType.Dog)
      whenever(dogs.nonEmpty) {
        val sub = subscriptionWithoutFreeUpgrade.saveMe()
        subscriptionBoxWithFreeUpgrade(sub).saveMe()

        ShipmentService.shouldSendFreeUpgradeShipment(sub, shipmentCount = 1, dogs) mustBe false

        clearTables()
      }
    }
  }

  it should "not send any upgrades on first shipment" in {
    forAllNoShrink(listOfNSimplePetsGen()) { pets =>
      val dogs = pets.filter(_.animalType.get == AnimalType.Dog)
      whenever(dogs.nonEmpty) {
        val sub = subscriptionWithFreeUpgrade.saveMe()
        subscriptionBoxWithFreeUpgrade(sub).saveMe()

        ShipmentService.shouldSendFreeUpgradeShipment(sub, shipmentCount = 0, dogs) mustBe false

        clearTables()
      }
    }
  }

  it should "not send any upgrades on third shipment" in {
    forAllNoShrink(listOfNSimplePetsGen()) { pets =>
      val dogs = pets.filter(_.animalType.get == AnimalType.Dog)
      whenever(dogs.nonEmpty) {
        val sub = subscriptionWithFreeUpgrade.saveMe()
        subscriptionBoxWithFreeUpgrade(sub).saveMe()

        ShipmentService.shouldSendFreeUpgradeShipment(sub, shipmentCount = 2, dogs) mustBe false

        clearTables()
      }
    }
  }

  it should "not send any upgrades on other shipments" in {
    forAllNoShrink(genPosInt, listOfNSimplePetsGen()) { (shipmentCount, pets) =>
      val dogs = pets.filter(_.animalType.get == AnimalType.Dog)
      whenever(shipmentCount > 2 && dogs.nonEmpty) {
        val sub = subscriptionWithFreeUpgrade.saveMe()
        subscriptionBoxWithFreeUpgrade(sub).saveMe()

        ShipmentService.shouldSendFreeUpgradeShipment(sub, shipmentCount, dogs) mustBe false

        clearTables()
      }
    }
  }

  it should "not send any upgrades when there are no dogs" in {
    val sub = subscriptionWithFreeUpgrade.saveMe()
    subscriptionBoxWithFreeUpgrade(sub).saveMe()

    ShipmentService.shouldSendFreeUpgradeShipment(sub, shipmentCount = 1, dogs = Nil) mustBe false
  }

  it should "not send any upgrades when customer is ineligible for them" in {
    forAllNoShrink(listOfNSimplePetsGen()) { pets =>
      val dogs = pets.filter(_.animalType.get == AnimalType.Dog)
      whenever(dogs.nonEmpty) {
        val sub = subscriptionWithoutFreeUpgrade.saveMe()

        ShipmentService.shouldSendFreeUpgradeShipment(sub, shipmentCount = 1, dogs) mustBe false

        clearTables()
      }
    }
  }

  private def setExpShipDateStatusAndShipmentStatus(
      in: InsertedUserSubAndShipment,
      expectedShipData: Date,
      shipmentStatus: ShipmentStatus.Value,
      status: Status.Value
  ): List[Shipment] =
    in.shipments.map(
      _.expectedShipDate(expectedShipData)
        .shipmentStatus(shipmentStatus)
        .status(status)
        .saveMe()
    )

  private def setStatusAndNextShipDate(
      in: InsertedUserAndSub,
      nextShipDate: Date,
      status: Status.Value
  ): Subscription =
    in.subscription.nextShipDate(nextShipDate).status(status).saveMe()

}

object ShipmentServiceSpec {
  val tppAgency: Agency = createAgency(tppAgencyName)

  val user: User =
    User.create
      .referer(tppAgency)
      .saveMe()

  val subscriptionWithFreeUpgrade: Subscription =
    Subscription.create
      .isUpgraded(false)
      .user(user)

  val subscriptionWithoutFreeUpgrade: Subscription =
    Subscription.create
      .isUpgraded(true)

  def subscriptionBoxWithFreeUpgrade(sub: Subscription): SubscriptionBox =
    SubscriptionBox.create
      .boxType(BoxType.basic)
      .subscription(sub)
}
