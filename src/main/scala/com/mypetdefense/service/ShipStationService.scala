package com.mypetdefense.service

import java.text.SimpleDateFormat
import java.time.{LocalDate, ZoneId}
import java.util.Date

import com.mypetdefense.model._
import com.mypetdefense.shipstation.{Address => ShipStationAddress, Shipment => ShipStationShipment, _}
import dispatch.Defaults._
import dispatch._
import net.liftweb.common.Box.tryo
import net.liftweb.common._
import net.liftweb.util.Props

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure => TryFail, Success => TrySuccess, _}

object ShipStationService extends Loggable {
  val key: String = Props.get("shipstation.key") openOr ""
  val secret: String = Props.get("shipstation.secret") openOr ""
  val url: String = Props.get("shipstation.url") openOr ""
  val dateFormat = new SimpleDateFormat("MM/dd/yyyy")

  implicit val shipStationExecutor: ShipStationExecutor = new ShipStationExecutor(key, secret, url)

  def getOrder(orderId: Int): Box[Order] = {
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
      Await.result(Order.list(List(("orderStatus","awaiting_shipment"))), new DurationInt(10).seconds)
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

  def createUserBillShipToAddress(user: User): ShipStationAddress = {
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

  def createOrderBillShipToAddress(order: TreatOrder): ShipStationAddress = {
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
    
    val treats = treatOrderLineItems.map { treat =>
      (treat.product.obj, treat.quantity.get)
    }

    val shipstationTreats = treats.map { case (treat, count) =>
      (treat.map(_.sku.get).openOr(""), count)
    }

    val shipStationProductIds = shipstationTreats ++ packaging.map(pkg => (pkg.sku.get, 1)).toList

    val shipStationItems = shipStationProductIds.map { case (sku, quantity) =>
      OrderItem(
        quantity = quantity,
        sku = sku
      )
    }

    val treatWeights = treats.map { case (treat, count) =>
      treat.map(_.weight.get).openOr(0D) * count
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

  def createShipStationOrder(shipment: Shipment, user: User, subscription: Subscription): Future[Box[Order]] = {
    val billShipTo = createUserBillShipToAddress(user)

    val shipmentLineItems = shipment.refresh.toList.flatMap(_.shipmentLineItems.toList)
    val someInserts = shipmentLineItems.flatMap(_.insert.obj).distinct

    val inserts =
      if (subscription.shipments.toList.size >= 2 && tryo(subscription.freeUpgradeSampleDate) == Full(null)) {
        val dogs = shipment.refresh.toList.flatMap(_.shipmentLineItems.flatMap(_.pet.obj).toList.distinct).filter(_.animalType.get == AnimalType.Dog)

        if (dogs.nonEmpty) {
          subscription.refresh.map(_.freeUpgradeSampleDate(new Date).saveMe())
          dogs.map(pet => ShipmentLineItem.sendFreeUpgradeItems(shipment, pet))

          Insert.tryUpgrade ++ someInserts
        } else
          someInserts
      } else
        someInserts

    val refreshedShipment = shipment.refresh
    val shipmentLineItemsByPet = refreshedShipment.toList.flatMap(_.shipmentLineItems.toList).groupBy(_.pet.obj).filter(_._1.isDefined)

    val fleaTick = shipmentLineItems.flatMap(_.fleaTick.obj)

    val productWeight = fleaTick.map(_.weight.get).sum
    val insertWeight = inserts.map(_.weight.get).sum

    val totalWeight = productWeight + insertWeight

    val normalizedWeight = if (totalWeight < 4.0) 4.0 else totalWeight

    val shipStationItems = inserts.toList.zipWithIndex.map { case (insert, index) =>
      OrderItem(
        lineItemKey = Some(s"9 - ${index}"),
        quantity = 1,
        sku = insert.itemNumber.get,
        name = s"0 - ${insert.name.get}"
      )
    } ++ shipmentLineItemsByPet.zipWithIndex.flatMap { case ((pet, lineItems), index) =>
      val fleaTick = lineItems.flatMap(_.fleaTick.obj)
      val products = lineItems.flatMap(_.product.obj)

      val fleaOrderItem = fleaTick.map { ft =>
        OrderItem(
          lineItemKey = Some(s"${index+1} - 9"),
          quantity = 1,
          sku = ft.sku.get,
          name = s"${index+1} - ${ft.getNameAndSize}"
        )
      }

      val productsOrderItems = products.zipWithIndex.map { case (product, productIndex) =>
        OrderItem(
          lineItemKey = Some(s"${index+1} - ${productIndex + 1}"),
          quantity = 1,
          sku = product.sku.get,
          name = s"${index+1} - ${product.name.get} for Dogs"
        )
      }

      List(OrderItem(
        lineItemKey = Some(s"${index + 1} - 0"),
        quantity = 1,
        sku = "pet",
        name = s"${index+1} -  ${pet.map(_.name.get).openOr("")}"
      )) ++ fleaOrderItem ++ productsOrderItems
    }

    Order.create(
      orderNumber = s"${refreshedShipment.map(_.shipmentId.get).openOr("")}",
      orderDate = dateFormat.format(new Date()),
      orderStatus = "awaiting_shipment",
      billTo = billShipTo,
      shipTo = billShipTo,
      items = Some(shipStationItems.sortBy(_.lineItemKey)),
      weight = Some(Weight(normalizedWeight, "ounces")),
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
    val yesterdayDate = Date.from(LocalDate.now(ZoneId.of("America/New_York")).atStartOfDay(ZoneId.of("America/New_York")).minusDays(1).toInstant())
    val shipDate = dateFormat.format(yesterdayDate)

    Try(
      Await.result(ShipStationShipment.list(
        List(
          ("shipDateStart", shipDate),
          ("shipDateEnd", shipDate),
          ("pageSize", "300")
      )), new DurationInt(10).seconds)
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
}
