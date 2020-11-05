package com.mypetdefense.jobs

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.helpers.GeneralDbUtils.insertUserSubAndShipment
import com.mypetdefense.model.{Shipment, ShipmentStatus, Subscription, User}
import com.mypetdefense.service.ShipStationServiceTrait
import com.mypetdefense.shipstation._
import net.liftweb.common.{Box, Full}
import org.quartz.JobExecutionContext
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import scala.collection.mutable
import scala.concurrent.Future

class CreateShipStationOrderJobSpec extends DBTest {

  private val someAddress = Address(street1 = "", city = "", state = "", postalCode = "")

  it should "create ship station orders in job" in {
    val data2insert  = shipmentChainData()
    val insertedData = insertUserSubAndShipment(data2insert)
    val insertedShipments = insertedData.shipments
      .map(_.shipmentStatus(ShipmentStatus.Paid).saveMe())
    val expectedIds          = for (id <- insertedShipments.indices) yield id
    val calledShipmentsIds   = mutable.Set.empty[Long]
    var calledUserId         = 0L
    var calledSubscriptionId = 0L

    def testFun(shipment: Shipment, user: User, subscription: Subscription, count: Int) = {
      calledUserId = user.id.get
      calledSubscriptionId = subscription.id.get
      calledShipmentsIds.add(shipment.id.get)
      defaultResponseOrderWithId(count)
    }

    val testJob = new CreateShipStationOrderJobTrait {
      override val shipStationService: ShipStationServiceTrait = testShipStationService(testFun)

      override def execute(context: JobExecutionContext): Unit = ()
    }

    testJob.createShipStationOrders()
    val updatedShipments = insertedShipments.flatMap(_.refresh)

    calledShipmentsIds should contain theSameElementsAs updatedShipments.map(_.id.get)
    updatedShipments.map(_.shipStationOrderId.get) should contain theSameElementsAs expectedIds
    calledUserId shouldBe insertedData.user.id.get
    calledSubscriptionId shouldBe insertedData.subscription.id.get
  }

  private def defaultResponseOrderWithId(id: Int): Box[Order] =
    Full[Order](
      Order(id, "", orderDate = "", orderStatus = "", billTo = someAddress, shipTo = someAddress)
    )

  private def testShipStationService(
      func: (Shipment, User, Subscription, Int) => Box[Order]
  ): ShipStationServiceTrait =
    new ShipStationServiceTrait {
      override def createShipStationOrder(
          shipment: Shipment,
          user: User,
          subscription: Subscription,
          count: Int
      ): Future[Box[Order]] = Future.successful(func(shipment, user, subscription, count))

      override implicit val shipStationExecutor: ShipStationExecutor =
        new ShipStationExecutor("", "")
    }

}
