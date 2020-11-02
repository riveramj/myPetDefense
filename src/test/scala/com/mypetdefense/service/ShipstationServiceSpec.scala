package com.mypetdefense.service

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers.db.AddressDbUtil.createAddress
import com.mypetdefense.shipstation._
import dispatch.{Future, Req}
import net.liftweb.common._
import org.scalatest.Assertions.fail
import org.scalatest.matchers.should.Matchers._

class ShipstationServiceSpec extends DBTest {

  it should "create ship station order" in {
    var evidence    = false
    val mpdAndPld   = createPetlandAndMPDAgencies()
    val userAddress = address()
    val data        = petsAndShipmentChainDataGen()
    val inserted =
      insertPetAndShipmentsChainAtAgency(data, mpdAndPld.mpd, subUpgraded = false)
    val insertedUser = inserted.user
    createAddress(userAddress, insertedUser)

    def testFun(in: Map[String, String]): Unit = {
      in.get("orderStatus").fold(fail("there is no order status in request")) { oStatus =>
        oStatus shouldBe "awaiting_shipment"
      }
      in.get("serviceCode").fold(fail("there is no service code in request")) { serviceCode =>
        serviceCode shouldBe "usps_first_class_mail"
      }
      in.get("customerEmail").fold(fail("there is no customer email in request")) { customerEmail =>
        customerEmail shouldBe inserted.user.email.get
      }
      in.get("street1").fold(fail("there is no street1 field in request")) { street1 =>
        street1.toLowerCase shouldBe userAddress.street1.toLowerCase
      }
      in.get("street2").fold(fail("there is no street2 field in request")) { street2 =>
        street2.toLowerCase shouldBe userAddress.street2.toLowerCase
      }
      in.get("postalCode").fold(fail("there is no postal code field in request")) { postalCode =>
        postalCode.toLowerCase shouldBe userAddress.zip.toLowerCase
      }
      in.get("state").fold(fail("there is no state field in request")) { state =>
        state.toLowerCase shouldBe userAddress.state.toLowerCase
      }
      evidence = true
    }

    val shipment    = inserted.shipments.head
    val testService = new TestShipStationService(testFun)
    testService.createShipStationOrder(shipment, insertedUser, inserted.subscription, 1)
    succeed
  }
}

class TestShipStationService(onRequest: Map[String, String] => Unit)
    extends ShipStationServiceTrait {
  override implicit val shipStationExecutor: ShipStationExecutor = new ShipStationExecutor("", "") {
    val someAddress: Address = Address(street1 = "", city = "", state = "", postalCode = "")
    override def executeFor[T <: ShipStationObject](
        request: Req
    )(implicit mf: Manifest[T]): Future[Box[T]] = {
      val mapRequest = request.toRequest.getStringData
        .replace("{", "")
        .replace("}", "")
        .split(",")
        .map { string =>
          val array = string.split(":").map(_.replace("\"", ""))
          (array.head, array.tail.head)
        }
        .toMap
      onRequest(mapRequest)
      Future.successful(
        Full(
          Order(1, "", orderDate = "", orderStatus = "", billTo = someAddress, shipTo = someAddress)
        ).asA[T]
      )
    }
  }
}
