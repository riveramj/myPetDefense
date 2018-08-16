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
import java.util.Base64
import java.nio.charset.StandardCharsets
import scala.util.{Failure => TryFail, Success => TrySuccess, _}

import com.mypetdefense.shipstation.{Address => ShipStationAddress, _}
import com.mypetdefense.model._

object ShipStationService extends Loggable {
  val key = Props.get("shipstation.key") openOr ""
  val secret = Props.get("shipstation.secret") openOr ""
  val url = Props.get("shipstation.secret") openOr ""

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

  def createShipStationOrder(shipment: Shipment, user: User) = {
    val userAddress = user.shippingAddress
    val billShipTo = ShipStationAddress(
      street1 = userAddress.map(_.street1.get).openOr(""),
      street2 = userAddress.map(_.street1.get),
      city = userAddress.map(_.city.get).openOr(""),
      state = userAddress.map(_.state.get).openOr(""),
      postalCode = userAddress.map(_.zip.get).openOr("")
    )

    val newOrder = Order.create(
      orderNumber = s"${shipment.shipmentId}",
      orderDate = new Date().toString,
      shipByDate = Some(shipment.expectedShipDate.get.toString),
      orderStatus = "on_hold",
      billTo = billShipTo,
      shipTo = billShipTo
    )

    Try (
      Await.result(newOrder, new DurationInt(10).seconds)
    ) match {
      case TrySuccess(Full(shipStationOrder)) =>
        Full(shipStationOrder)

      case TrySuccess(shipStationFailure) =>
        shipStationFailure

      case TryFail(throwable: Throwable) =>
        Empty
    }  
  }
}
