package com.mypetdefense.service

import net.liftweb._ 
  import common._
  import util.Helpers.tryo
  import json._
  import util.Props

import dispatch._, Defaults._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

import scala.collection.concurrent.TrieMap
import scala.math.BigDecimal

import java.util.Date
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import java.time.{LocalDate, ZoneId, LocalDateTime, Period}
import java.time.format.DateTimeFormatter

import scala.util.{Failure => TryFail, Success => TrySuccess, _}

import com.mypetdefense.shipstation.{Address => ShipStationAddress, Shipment => ShipStationShipment, _}
import com.mypetdefense.model._

object ShipStationService extends Loggable {
  val key = Props.get("shipstation.key") openOr ""
  val secret = Props.get("shipstation.secret") openOr ""
  val url = Props.get("shipstation.url") openOr ""
  val dateFormat = new SimpleDateFormat("MM/dd/yyyy")

  implicit val shipStationExecutor = new ShipStationExecutor(key, secret, url)

  def getOrder(orderId: Int) = {
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
  ) = {
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

  def cancelShipstationOrder(shipment: Shipment) = {
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

  def createUserBillShipToAddress(user: User) = {
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

  def createOrderBillShipToAddress(order: TreatOrder) = {
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
    val treats = treatOrderLineItems.flatMap(_.treat.obj)
    val shipStationProductIds = treats.map(_.sku.get) ++ packaging.map(_.sku.get).toList

    val shipStationItems = shipStationProductIds.map { sku =>
      OrderItem(
        quantity = 1,
        sku = sku
      )
    }

    val totalWeight = treats.map(_.weight.get).sum + packaging.map(_.weight.get).openOr(0.0)
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
    val tags = subscription.tags.toList.map(_.tag.get)

    val useBox = Tag.useBox.map(tags.contains(_)).openOr(false)

    val billShipTo = createUserBillShipToAddress(user)

    val refreshedShipment = shipment.refresh
    val shipmentLineItems = refreshedShipment.toList.map(_.shipmentLineItems.toList).flatten
    val shipmentProducts = shipmentLineItems.filter(!_.fleaTick.obj.isEmpty)
    
    val petNamesProducts = shipmentProducts.map(_.getFleaTickPetNameItemSize).mkString(". ")

    val products = shipmentLineItems.map(_.fleaTick.obj).flatten
    val inserts = shipmentLineItems.map(_.insert.obj).flatten

    val shipStationProductIds = products.map(_.sku.get)

    val paidShipment_? = tryo(shipment.amountPaid.get.toDouble).openOr(0.0) > 0.0

    val fleaTickCount = products.size

    val (packaging, packagingId) = (useBox, fleaTickCount) match {
      case (true, count) if count < 6 =>
        val packaging = Packaging.getSmallBox
        (packaging, packaging.map(_.sku.get).toList)

      case (true, _) =>
        val packaging = Packaging.getLargeBox
        (packaging, packaging.map(_.sku.get).toList)

      case (false, 1 | 2 | 3) => 
        (Packaging.getBubbleMailer, Nil)
      
      case (false, 4 | 5) => 
        val packaging = Packaging.getSmallBox
        (packaging, packaging.map(_.sku.get).toList)
  
      case (_, _) => 
        Packaging.getLargeBox
        val packaging = Packaging.getSmallBox
        (packaging, packaging.map(_.sku.get).toList)
    }

    val productWeight = products.map(_.weight.get).sum
    val insertWeight = inserts.map(_.weight.get).sum

    val totalWeight = productWeight + insertWeight + packaging.map(_.weight.get).openOr(0.0)

    val normalizedWeight = if (totalWeight < 4.0) 4.0 else totalWeight

    val shipStationInsertsIds = inserts.map(_.itemNumber.get)

    val allShipStationItems = shipStationProductIds ++ shipStationInsertsIds ++ packagingId

    val shipStationItems = allShipStationItems.map { sku =>
      OrderItem(
        quantity = 1,
        sku = sku
      )
    }

    Order.create(
      orderNumber = s"${refreshedShipment.map(_.shipmentId.get).openOr("")}",
      orderDate = dateFormat.format(new Date()),
      orderStatus = "awaiting_shipment",
      billTo = billShipTo,
      shipTo = billShipTo,
      items = Some(shipStationItems),
      giftMessage = Some(petNamesProducts),
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

  def getYesterdayShipments() = {
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
