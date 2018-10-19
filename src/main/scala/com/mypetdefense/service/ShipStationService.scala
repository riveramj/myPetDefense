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

  def createShipStationOrder(shipment: Shipment, user: User): Future[Box[Order]] = {
    val userAddress = user.shippingAddress
    val billShipTo = ShipStationAddress(
      name = Some(user.name),
      street1 = userAddress.map(_.street1.get).openOr(""),
      street2 = userAddress.map(_.street2.get),
      city = userAddress.map(_.city.get).openOr(""),
      state = userAddress.map(_.state.get).openOr("").toUpperCase,
      postalCode = userAddress.map(_.zip.get).openOr("")
    )

    val refreshedShipment = shipment.refresh
    val shipmentLineItems = refreshedShipment.toList.map(_.shipmentLineItems.toList).flatten
    
    val petNamesProducts = shipmentLineItems.map(_.getShipmentItem).mkString(". ")

    val products = shipmentLineItems.map(_.product.obj).flatten

    val shipStationProductIds = products.map { product =>

      product.getNameAndSize match {
        case name if (name.contains("ZoGuard Plus for Dogs 4-22")) =>
          "zgSMall"

        case name if (name.contains("ZoGuard Plus for Dogs 23-44")) =>
          "ZgMedium"

        case name if (name.contains("ZoGuard Plus for Dogs 45-88")) =>
          "ZgLarge"

        case other =>
          other
      }
    }

    val paidShipment_? = shipment.amountPaid != "0"

    val dogTagOrderItems = {
      if (paidShipment_?) {
        val dogProducts = products.map(_.getNameAndSize).filter(_.contains("Dogs"))
        dogProducts.map { product =>
          OrderItem(
            quantity = 1,
            sku = "dogTag"
          )
        }
      } else {
        Nil
      }
    }

    val possibleInsertOrderItem = refreshedShipment.map(_.insert.get) match {
      case Full("TPP+Welcome Insert") => 
        List(OrderItem(
          quantity = 1,
          sku = "WelcomeTPP"
        ))
      case _ => Nil
    }

    val shipStationProducts = shipStationProductIds.map { sku =>
      OrderItem(
        quantity = 1,
        sku = sku
      )
    }

    val allOrderItems = shipStationProducts ++ possibleInsertOrderItem ++ dogTagOrderItems

    Order.create(
      orderNumber = s"${refreshedShipment.map(_.shipmentId.get).openOr("")}",
      orderDate = dateFormat.format(new Date()),
      orderStatus = "awaiting_shipment",
      billTo = billShipTo,
      shipTo = billShipTo,
      items = Some(allOrderItems),
      giftMessage = Some(petNamesProducts)
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
