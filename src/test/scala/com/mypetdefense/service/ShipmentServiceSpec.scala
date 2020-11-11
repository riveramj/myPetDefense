package com.mypetdefense.service

import java.util.Date
import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers.Random._
import com.mypetdefense.model._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import net.liftweb.common.Full

class ShipmentServiceSpec extends DBTest {

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

  it should "get current past due shipments" in {
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

  it should "get upcoming subscriptions" in {
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

  it should "get past due shipments" in {
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
