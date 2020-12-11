package com.mypetdefense.service

import java.text.SimpleDateFormat
import java.time.{LocalDate, ZoneId}
import java.util.Date

import com.mypetdefense.model._
import com.mypetdefense.shipstation.{
  Address => ShipStationAddress,
  Shipment => ShipStationShipment,
  _
}
import com.mypetdefense.util.CalculationHelper
import com.mypetdefense.util.ModelSyntax.ListOfShipmentLineItemsSyntax
import dispatch.Defaults._
import dispatch._
import net.liftweb.common.Box.tryo
import net.liftweb.common._
import net.liftweb.util.Props

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure => TryFail, Success => TrySuccess, _}

trait ShipStationServiceTrait extends Loggable {
  val key: String    = Props.get("shipstation.key") openOr ""
  val secret: String = Props.get("shipstation.secret") openOr ""
  val url: String    = Props.get("shipstation.url") openOr ""
  val dateFormat     = new SimpleDateFormat("MM/dd/yyyy")

  implicit val shipStationExecutor: ShipStationExecutor

  private def getOrder(orderId: Int): Box[Order] = {
    Try(
      Await.result(Order.get(orderId.toString), new DurationInt(10).seconds)
    ) match {
      case TrySuccess(Full(shipStationOrder)) =>
        Full(shipStationOrder)

      case TrySuccess(shipStationFailure) =>
        logger.error(s"get order failed with shipStation error: ${shipStationFailure}")
        shipStationFailure

      case TryFail(throwable: Throwable) =>
        logger.error(s"get order failed with other error: ${throwable}")
        Empty
    }
  }

  def getReadyOrders(): Box[OrderList] = {
    Try(
      Await
        .result(Order.list(List(("orderStatus", "awaiting_shipment"))), new DurationInt(10).seconds)
    ) match {
      case TrySuccess(Full(shipStationOrder)) =>
        Full(shipStationOrder)

      case TrySuccess(shipStationFailure) =>
        logger.error(s"get order failed with shipStation error: ${shipStationFailure}")
        shipStationFailure

      case TryFail(throwable: Throwable) =>
        logger.error(s"get order failed with other error: ${throwable}")
        Empty
    }
  }

  def createOrderLabel(
      orderId: Int,
      carrierCode: String,
      serviceCode: String,
      packageCode: String,
      shipDate: String,
      weight: Weight,
      testLabel: Boolean = false
  ): Box[Label] = {
    val createLabel = Order.createLabelForOrder(
      orderId = orderId,
      carrierCode = carrierCode,
      serviceCode = serviceCode,
      packageCode = packageCode,
      shipDate = shipDate,
      weight = Some(weight),
      testLabel = testLabel
    )

    Try(
      Await.result(createLabel, new DurationInt(10).seconds)
    ) match {
      case TrySuccess(Full(shipStationLabel)) =>
        Full(shipStationLabel)

      case TrySuccess(shipStationFailure) =>
        logger.error(s"get order failed with shipStation error: ${shipStationFailure}")
        shipStationFailure

      case TryFail(throwable: Throwable) =>
        logger.error(s"get order failed with other error: ${throwable}")
        Empty
    }
  }

  def cancelShipstationOrder(shipment: Shipment): Box[Order] = {
    val possibleOrder = getOrder(shipment.shipStationOrderId.get)

    val cancelOrder = possibleOrder.map { order =>
      Order.create(
        orderNumber = order.orderNumber,
        orderKey = order.orderKey,
        orderDate = order.orderDate,
        orderStatus = "cancelled",
        billTo = order.billTo,
        shipTo = order.shipTo
      )
    }.openOr(Future(Empty))

    Try(Await.result(cancelOrder, new DurationInt(10).seconds)) match {
      case TrySuccess(Full(order)) =>
        Full(order)

      case TrySuccess(shipStationFailure) =>
        logger.error(s"cancel order failed with shipStation error: ${shipStationFailure}")
        shipStationFailure

      case TryFail(throwable: Throwable) =>
        logger.error(s"cancel order failed with other error: ${throwable}")
        Empty
    }
  }

  private def createUserBillShipToAddress(user: User): ShipStationAddress = {
    val userAddress = user.shippingAddress

    ShipStationAddress(
      name = Some(user.name),
      street1 = userAddress.map(_.street1.get).openOr(""),
      street2 = userAddress.map(_.street2.get),
      city = userAddress.map(_.city.get).openOr(""),
      state = userAddress.map(_.state.get).openOr("").toUpperCase,
      postalCode = userAddress.map(_.zip.get).openOr("")
    )
  }

  private def createOrderBillShipToAddress(order: TreatOrder): ShipStationAddress = {
    ShipStationAddress(
      name = Some(order.name),
      street1 = order.street1.get,
      street2 = Some(order.street2.get),
      city = order.city.get,
      state = order.state.get.toUpperCase,
      postalCode = order.zip.get
    )
  }

  def createShipStationTreatOrder(order: TreatOrder): Future[Box[Order]] = {
    val packaging = Packaging.getLargeBox

    val billShipTo = createOrderBillShipToAddress(order)

    val treatOrderLineItems: List[TreatOrderLineItem] = order.treatsOrdered.toList

    val treats = treatOrderLineItems.map { treat => (treat.product.obj, treat.quantity.get) }

    val shipstationTreats = treats.map {
      case (treat, count) =>
        (treat.map(_.sku.get).openOr(""), count)
    }

    val shipStationProductIds = shipstationTreats ++ packaging.map(pkg => (pkg.sku.get, 1)).toList

    val shipStationItems = shipStationProductIds.map {
      case (sku, quantity) =>
        OrderItem(
          quantity = quantity,
          sku = sku
        )
    }

    val treatWeights = treats.map {
      case (treat, count) =>
        treat.map(_.weight.get).openOr(0d) * count
    }

    val totalWeight = treatWeights.sum + packaging.map(_.weight.get).openOr(0.0)
    val carrierCode = "stamps_com"
    val serviceCode = if (treats.size == 1) "usps_first_class_mail" else "usps_priority_mail"
    val packageCode = "package"

    Order.create(
      orderNumber = s"${order.treatOrderId.get}",
      orderDate = dateFormat.format(new Date()),
      orderStatus = "awaiting_shipment",
      billTo = billShipTo,
      shipTo = billShipTo,
      items = Some(shipStationItems),
      weight = Some(Weight(totalWeight, "ounces")),
      carrierCode = Some(carrierCode),
      serviceCode = Some(serviceCode),
      packageCode = Some(packageCode)
    )

  }

  def createShipStationOrder(
      shipment: Shipment,
      user: User,
      subscription: Subscription,
      count: Int
  ): Future[Box[Order]] = {
    val billShipTo = createUserBillShipToAddress(user)

    val shipmentLineItems        = shipment.actualShipmentLineItems
    val shipmentLineItemsInserts = shipmentLineItems.distinctInserts

    val allInserts = insertsToShipStation(subscription, shipment, shipmentLineItemsInserts)

    val refreshedShipment      = shipment.reload
    val shipmentLineItemsByPet = refreshedShipment.shipmentLineItemsByPets

    val fleaTick = shipmentLineItems.flatMap(_.fleaTick.obj)

    val normalizedWeight = CalculationHelper.calculateInsertsWeight(fleaTick, allInserts)

    val shipStationItems = allInserts.toList.zipWithIndex.map {
      case (insert, index) => insertToOrderItem(insert, index)
    } ++ shipmentLineItemsByPet.zipWithIndex.flatMap {
      case ((pet, lineItems), index) => petFleaTickAndProductsToOrderItems(pet, lineItems, index)
    }

    Order.create(
      orderNumber = s"${refreshedShipment.shipmentId.get}",
      orderDate = dateFormat.format(new Date()),
      orderStatus = "awaiting_shipment",
      billTo = billShipTo,
      shipTo = billShipTo,
      items = Some(shipStationItems.sortBy(_.lineItemKey)),
      weight = Some(Weight(normalizedWeight.toDouble, "ounces")),
      carrierCode = Some("stamps_com"),
      serviceCode = Some("usps_first_class_mail"),
      packageCode = Some("package"),
      customerEmail = Some(user.email.get)
    )
  }

  def holdOrderUntil(orderId: Int, date: Date): Future[Box[HoldOrderResults]] = {
    val holdDate = dateFormat.format(date)

    Order.holdUntil(orderId, holdDate)
  }

  def getYesterdayShipments(): Box[ShipmentList] = {
    val dateFormat = new SimpleDateFormat("MM/dd/yyyy")
    val yesterdayDate = Date.from(
      LocalDate
        .now(ZoneId.of("America/New_York"))
        .atStartOfDay(ZoneId.of("America/New_York"))
        .minusDays(1)
        .toInstant()
    )
    val shipDate = dateFormat.format(yesterdayDate)

    Try(
      Await.result(
        ShipStationShipment.list(
          List(
            ("shipDateStart", shipDate),
            ("shipDateEnd", shipDate),
            ("pageSize", "300")
          )
        ),
        new DurationInt(10).seconds
      )
    ) match {
      case TrySuccess(Full(shipStationShipments)) =>
        Full(shipStationShipments)

      case TrySuccess(shipStationFailure) =>
        logger.error(s"get order failed with shipStation error: ${shipStationFailure}")
        shipStationFailure

      case TryFail(throwable: Throwable) =>
        logger.error(s"get order failed with other error: ${throwable}")
        Empty
    }
  }

  private def insertsToShipStation(
      subscription: Subscription,
      shipment: Shipment,
      shipmentLineItemsInserts: List[Insert]
  ): Iterable[Insert] = {
    if (subscription.shipments.toList.size >= 2 &&
        tryo(subscription.freeUpgradeSampleDate) == Full(null)) {
      insertsWithFreeUpgrade(subscription, shipment, shipmentLineItemsInserts)
    } else if (shipment.freeUpgradeSample.get)
      Insert.tryUpgrade ++ shipmentLineItemsInserts
    else
      shipmentLineItemsInserts
  }

  private def insertsWithFreeUpgrade(
      subscription: Subscription,
      shipment: Shipment,
      shipmentLineItemsInserts: List[Insert]
  ) = {
    val dogs = shipment.reload.shipmentLineItems
      .flatMap(_.pet.obj)
      .toList
      .distinct
      .filter(_.animalType.get == AnimalType.Dog)

    if (dogs.nonEmpty) {
      subscription.reload.freeUpgradeSampleDate(new Date).saveMe()
      dogs.map(pet => ShipmentLineItem.sendFreeUpgradeItems(shipment, pet))

      Insert.tryUpgrade ++ shipmentLineItemsInserts
    } else
      shipmentLineItemsInserts
  }

  private def productWithIndexAndProductIndexToOrderItem(
      product: Product,
      index: Int,
      productIndex: Int
  ): OrderItem =
    OrderItem(
      lineItemKey = Some(s"${index + 1} - ${productIndex + 1}"),
      quantity = 1,
      sku = product.sku.get,
      name = s"${index + 1} - ${product.name.get} for Dogs"
    )

  private def petFleaTickAndProductsToOrderItems(
      pet: Pet,
      lineItems: List[ShipmentLineItem],
      index: Int
  ) = {
    val fleaTick = lineItems.flatMap(_.fleaTick.obj)
    val products = lineItems.flatMap(_.product.obj)

    val fleaOrderItem = fleaTick.map(fleaTickToOrderItem(_, index))

    val productsOrderItems = products.zipWithIndex.map {
      case (product, productIndex) =>
        productWithIndexAndProductIndexToOrderItem(product, index, productIndex)
    }

    List(petToOrderItem(pet, index)) ++ fleaOrderItem ++ productsOrderItems
  }

  private def fleaTickToOrderItem(input: FleaTick, index: Int) =
    OrderItem(
      lineItemKey = Some(s"${index + 1} - 9"),
      quantity = 1,
      sku = input.sku.get,
      name = s"${index + 1} - ${input.getNameAndSize}"
    )

  private def petToOrderItem(input: Pet, index: Int) = OrderItem(
    lineItemKey = Some(s"${index + 1} - 0"),
    quantity = 1,
    sku = "pet",
    name = s"${index + 1} -  ${input.name.get}"
  )

  private def insertToOrderItem(input: Insert, index: Int) = OrderItem(
    lineItemKey = Some(s"9 - $index"),
    quantity = 1,
    sku = input.itemNumber.get,
    name = s"0 - ${input.name.get}"
  )

}

object ShipStationService extends ShipStationServiceTrait {
  override implicit val shipStationExecutor: ShipStationExecutor =
    new ShipStationExecutor(key, secret, url)
}
