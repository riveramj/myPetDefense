package com.mypetdefense.service

import com.mypetdefense.generator.Generator._
import com.mypetdefense.generator.{AddressGeneratedData, InsertGenData}
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.helpers.DateUtil.{ZonedDateTimeSyntax, yesterday}
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers.Random.randomPosInt
import com.mypetdefense.helpers.db.AddressDbUtil._
import com.mypetdefense.helpers.db.InsertsDbHelper._
import com.mypetdefense.model.{BoxType, Pet, Subscription}
import com.mypetdefense.shipstation._
import dispatch.{Future, Req}
import net.liftweb.common._
import net.liftweb.http.rest._
import net.liftweb.json._
import net.liftweb.mapper.By
import org.asynchttpclient.Param
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers._

import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.JavaConverters._

class ShipstationServiceSpec extends DBTest with RestHelper {

  private val expDateFormat = new SimpleDateFormat("MM/dd/yyyy")
  private val someAddress   = Address(street1 = "", city = "", state = "", postalCode = "")
  private val defaultResponseOrder: Box[ShipStationObject] =
    Full[ShipStationObject](
      Order(1, "", orderDate = "", orderStatus = "", billTo = someAddress, shipTo = someAddress)
    )

  it should "create ship station order" in {
    var evidence      = false
    val mpdAndPld     = createPetlandAndMPDAgencies()
    val userAddress   = address()
    val userAndPet    = petsAndShipmentChainDataGen()
    val insertGenData = insertData()
    val inserts       = insertGenData.map(createInsert)
    val inserted =
      insertPetAndShipmentsChainAtAgency(userAndPet, mpdAndPld.mpd, subUpgraded = false, inserts)
    val insertedUser = inserted.user
    createAddress(userAddress, insertedUser)

    val insertedSub = Subscription
      .find(By(Subscription.id, insertedUser.subscription.get))
      .openOrThrowException("oops")

    val shipment = inserted.shipments.head.reload

    val expectedWeight = insertedSub.subscriptionBoxes
      .map(b =>
        b.boxType.get match {
          case BoxType.basic if !shipment.freeUpgradeSample.get => BigDecimal(5)
          case BoxType.basic             => BigDecimal(15)
          case BoxType.`complete` => BigDecimal(15)
          case _                         => BigDecimal(0)
        }
      )
      .sum

    val serviceCode = if (expectedWeight.toDouble < 16D)
      "usps_first_class_mail"
    else
      "usps_parcel_select"

    def jsonAssertFun(maybeIn: Option[JValue]): Unit = {
      maybeIn.fold(fail("json is empty")) { in =>
        in \ "orderStatus" shouldBe JString("awaiting_shipment")
        in \ "serviceCode" shouldBe JString(serviceCode)
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
    }

    val testService = new TestShipStationService(onRequest = assertRequestOnExecFor(jsonAssertFun))
    testService.createShipStationOrder(shipment, insertedUser, inserted.subscription, 1)
    evidence shouldBe true
  }

  it should "cancel shipstation order properly" in {
    val mpdAndPld   = createPetlandAndMPDAgencies()
    val userAndPet  = petsAndShipmentChainDataGen()
    val orderNumber = randomPosInt
    val inserted =
      insertPetAndShipmentsChainAtAgency(userAndPet, mpdAndPld.mpd, subUpgraded = false)
    val shipmentToDelete = inserted.shipments.head.shipStationOrderId(orderNumber).saveMe()
    def requestAssertFun(maybeIn: Option[JValue]): Unit = {
      maybeIn.fold(succeed) { in =>
        (in \ "orderStatus").extract[String] shouldBe "cancelled"
        (in \ "orderNumber").extract[String] shouldBe orderNumber.toString
      }
    }
    val orderResponse = Order(
      1,
      orderNumber.toString,
      orderDate = "",
      orderStatus = "",
      billTo = someAddress,
      shipTo = someAddress
    )

    val testService = new TestShipStationService(
      onRequest = assertRequestOnExecFor(requestAssertFun, Full(orderResponse))
    )

    testService.cancelShipstationOrder(shipmentToDelete)
    succeed
  }

  it should "get yesterday shipments" in {
    var evidenceSize  = false
    var evidenceStart = false
    var evidenceEnd   = false
    def queryParamsAssertFun(maybeIn: List[Param]): Unit = {
      val yesterdayFormatted = expDateFormat.format(yesterday.toDate).replaceAll("/", "%2F")
      evidenceSize = maybeIn.find(_.getName == "pageSize").exists(_.getValue == "300")
      evidenceStart =
        maybeIn.find(_.getName == "shipDateStart").exists(_.getValue == yesterdayFormatted)
      evidenceEnd =
        maybeIn.find(_.getName == "shipDateEnd").exists(_.getValue == yesterdayFormatted)
    }
    val testService = new TestShipStationService(
      onRequest = assertQueryParamsOnExecFor(queryParamsAssertFun)
    )
    testService.getYesterdayShipments()

    evidenceSize shouldBe true
    evidenceStart shouldBe true
    evidenceEnd shouldBe true
  }

  private def compareItems(in: JValue, inserts: List[InsertGenData], pets: List[Pet]): Assertion = {
    val itemsList  = in.extract[List[JValue]]
    val itemsNames = itemsList.map(_ \ "name").map(_.extract[String])
    itemsNames.size shouldBe ((pets.size * 2) + inserts.size) // x 2 because of products and pet name
    inserts.forall(insert => itemsNames.exists(_.contains(insert.name))) shouldBe true
    pets.forall(pet => itemsNames.exists(_.contains(pet.name.get))) shouldBe true
  }

  private def assertRequestOnExecFor[T <: ShipStationObject](
      jsonAssert: Option[JValue] => Unit,
      response: Box[ShipStationObject] = defaultResponseOrder
  )(req: Req): Box[ShipStationObject] = {
    val requestString = Option(req.toRequest.getStringData)
    val requestJson   = requestString.map(parse)
    jsonAssert(requestJson)
    response
  }

  private def assertQueryParamsOnExecFor[T <: ShipStationObject](
      queryParamsAssert: List[Param] => Unit
  )(req: Req): Box[ShipStationObject] = {
    val params = req.toRequest.getQueryParams.asScala.toList
    queryParamsAssert(params)
    defaultResponseOrder
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

class TestShipStationService(onRequest: Req => Box[ShipStationObject])
    extends ShipStationServiceTrait {
  override implicit val shipStationExecutor: ShipStationExecutor = new ShipStationExecutor("", "") {
    override def executeFor[T <: ShipStationObject](request: Req)(
        implicit mf: Manifest[T]
    ): Future[Box[T]] = Future.successful(onRequest(request).asA[T])
  }
}
