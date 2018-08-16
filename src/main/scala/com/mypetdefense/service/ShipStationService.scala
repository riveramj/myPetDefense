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
import scala.util.{Failure => TryFail, Success => TrySuccess, _}

import com.mypetdefense.shipstation.{Address => ShipStationAddress, _}
import com.mypetdefense.model._

object ShipStationService extends Loggable {
  val key = Props.get("shipstation.key") openOr ""
  val secret = Props.get("shipstation.secret") openOr ""
  val url = Props.get("shipstation.url") openOr ""
  val dateFormat = new SimpleDateFormat("MM/dd/yyyy")

  implicit val shipStationExecutor = new ShipStationExecutor(key, secret, url)

  def getOrder(orderId: String) = {
    Try(
      Await.result(Order.get(orderId), new DurationInt(10).seconds)
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

  def createShipStationOrder(oldShipment: Shipment, user: User) = {
    val userAddress = user.shippingAddress
    val billShipTo = ShipStationAddress(
      name = Some(user.name),
      street1 = userAddress.map(_.street1.get).openOr(""),
      street2 = userAddress.map(_.street2.get),
      city = userAddress.map(_.city.get).openOr(""),
      state = userAddress.map(_.state.get).openOr(""),
      postalCode = userAddress.map(_.zip.get).openOr("")
    )

    val shipment = oldShipment.refresh
    val shipmentLineItems = shipment.toList.map(_.shipmentLineItems.toList).flatten
    
    val petNamesProducts = shipmentLineItems.map(_.getShipmentItem).mkString(". ")

    val products = shipmentLineItems.map(_.product.obj).flatten

    println("========")
    println(products)
    println("========")
    println("========")
    println(products.map(_.getNameAndSize))
    println("========")

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

    val possibleInsertOrderItem = shipment.map(_.insert.get) match {
      case Full("TPP+Welcome Insert") => 
        List(OrderItem(
          quantity = 1,
          sku = "WelcomeTPP"
        ))
      case _ => Nil
    }

    println("==== sku")
    println(shipStationProductIds)
    println("==== sku")

    val shipStationProducts = shipStationProductIds.map { sku =>
      OrderItem(
        quantity = 1,
        sku = sku
      )
    }

    val allOrderItems = shipStationProducts ++ possibleInsertOrderItem

    println("==== order items")
    println(allOrderItems)
    println("==== order items")

    val newOrder = Order.create(
      orderNumber = s"${shipment.map(_.shipmentId.get).openOr("")}",
      orderDate = dateFormat.format(new Date()),
      shipByDate = shipment.map(s => dateFormat.format(s.expectedShipDate.get)),
      orderStatus = "on_hold",
      billTo = billShipTo,
      shipTo = billShipTo,
      items = Some(allOrderItems),
      customerNotes = Some(petNamesProducts)
    )

    Try (
      Await.result(newOrder, new DurationInt(10).seconds)
    ) match {
      case TrySuccess(Full(shipStationOrder)) =>
        logger.error("created Order")
        logger.error(shipStationOrder)
        Full(shipStationOrder)

      case TrySuccess(shipStationFailure) =>
        logger.error("ShipStation failure")
        logger.error(shipStationFailure)
        shipStationFailure

      case TryFail(throwable: Throwable) =>
        logger.error("errror:")
        logger.error(throwable)
        Empty
    }  
  }
}
