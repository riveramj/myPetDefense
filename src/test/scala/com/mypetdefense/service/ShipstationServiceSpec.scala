package com.mypetdefense.service

import java.text.SimpleDateFormat
import java.util.Date
import com.mypetdefense.generator.{AddressGeneratedData, InsertGenData}
import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers.db.AddressDbUtil._
import com.mypetdefense.helpers.db.InsertsDbHelper._
import com.mypetdefense.model.Pet
import com.mypetdefense.shipstation._
import dispatch.{Future, Req}
import net.liftweb.common._
import net.liftweb.http.rest._
import net.liftweb.json._
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers._

class ShipstationServiceSpec extends DBTest with RestHelper {

  it should "create ship station order" in {
    var evidence      = false
    val expDateFormat = new SimpleDateFormat("MM/dd/yyyy")
    val mpdAndPld     = createPetlandAndMPDAgencies()
    val userAddress   = address()
    val userAndPet    = petsAndShipmentChainDataGen()
    val insertGenData = insertData()
    val inserts       = insertGenData.map(createInsert)
    val inserted =
      insertPetAndShipmentsChainAtAgency(userAndPet, mpdAndPld.mpd, subUpgraded = false, inserts)
    val insertedUser = inserted.user
    createAddress(userAddress, insertedUser)
    val insertsTotalWeight =
      insertGenData.foldLeft(BigDecimal(0d))((acc, elem) => acc + BigDecimal(elem.weight))
    val expectedWeight = if (insertsTotalWeight < 4.0) BigDecimal(4.0) else insertsTotalWeight
    val shipment       = inserted.shipments.head.refresh.toOption.get

    def testFun(in: JValue): Unit = {
      in \ "orderStatus" shouldBe JString("awaiting_shipment")
      in \ "serviceCode" shouldBe JString("usps_first_class_mail")
      in \ "carrierCode" shouldBe JString("stamps_com")
      in \ "orderDate" shouldBe JString(expDateFormat.format(new Date()))
      in \ "customerEmail" shouldBe JString(inserted.user.email.get)
      val addressBillTo = in \ "billTo"
      val addressShipTo = in \ "shipTo"
      val items         = in \ "items"
      val weightData    = in \ "weight"
      compareItems(items, insertGenData, inserted.pets)
      compareAddress(userAddress, insertedUser.name, addressBillTo)
      compareAddress(userAddress, insertedUser.name, addressShipTo)
      compareWeight(expectedWeight.toDouble, weightData)
      evidence = true
    }

    val testService = new TestShipStationService(testFun)
    testService.createShipStationOrder(shipment, insertedUser, inserted.subscription, 1)
    evidence shouldBe true
  }

  private def compareItems(in: JValue, inserts: List[InsertGenData], pets: List[Pet]): Assertion = {
    val itemsList  = in.extract[List[JValue]]
    val itemsNames = itemsList.map(_ \ "name").map(_.extract[String])
    itemsNames.size shouldBe ((pets.size * 2) + inserts.size) // x 2 because of flea tick
    inserts.forall(insert => itemsNames.exists(_.contains(insert.name))) shouldBe true
    pets.forall(pet => itemsNames.exists(_.contains(pet.name.get))) shouldBe true
  }

  private def compareWeight(expected: Double, actualData: JValue): Assertion = {
    BigDecimal((actualData \ "value").extract[Double]).rounded shouldBe BigDecimal(expected).rounded
    actualData \ "units" shouldBe JString("ounces")
  }

  private def compareAddress(
      expected: AddressGeneratedData,
      userName: String,
      actualData: JValue
  ): Assertion = {
    actualData \ "name" shouldBe JString(userName)
    actualData \ "street1" shouldBe JString(expected.street1.toLowerCase.capitalize)
    actualData \ "street2" shouldBe JString(expected.street2.toLowerCase.capitalize)
    actualData \ "postalCode" shouldBe JString(expected.zip.capitalize)
    actualData \ "state" shouldBe JString(expected.state.capitalize)
    actualData \ "city" shouldBe JString(expected.city.toLowerCase.capitalize)
  }

}

class TestShipStationService(onRequest: JValue => Unit) extends ShipStationServiceTrait {
  override implicit val shipStationExecutor: ShipStationExecutor = new ShipStationExecutor("", "") {
    val someAddress: Address = Address(street1 = "", city = "", state = "", postalCode = "")
    override def executeFor[T <: ShipStationObject](
        request: Req
    )(implicit mf: Manifest[T]): Future[Box[T]] = {
      val requestString = request.toRequest.getStringData
      val requestJson   = parse(requestString)
      onRequest(requestJson)
      Future.successful(
        Full(
          Order(1, "", orderDate = "", orderStatus = "", billTo = someAddress, shipTo = someAddress)
        ).asA[T]
      )
    }
  }
}
