package com.mypetdefense.service

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.helpers.GeneralDbUtils.insertUserAndSub
import com.mypetdefense.helpers.Random._
import com.mypetdefense.model.Shipment
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

}
