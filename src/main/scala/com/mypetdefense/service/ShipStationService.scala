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

  def createShipStationOrder(shipment: Shipment, user: User) = {
    val userAddress = user.shippingAddress
    val billShipTo = ShipStationAddress(
      name = Some(user.name),
      street1 = userAddress.map(_.street1.get).openOr(""),
      street2 = userAddress.map(_.street2.get),
      city = userAddress.map(_.city.get).openOr(""),
      state = userAddress.map(_.state.get).openOr(""),
      postalCode = userAddress.map(_.zip.get).openOr("")
    )

    val shipmentLineItems = shipment.shipmentLineItems.toList
    
    val petNamesProducts = shipmentLineItems.map { lineItem =>
      val lineItemPetName = lineItem.petName.get
      val lineItemProductName = lineItem.product.obj.map(_.getNameAndSize).openOr("")

      s"${lineItemPetName} - ${lineItemProductName}"
    }.mkString(". ")

    val products = shipmentLineItems.map { lineItem =>
      lineItem.product.obj
    }.flatten

    val shipStationProductIds = products.map { product =>

      product.getNameAndSize match {
        case name if (name.contains("ZoGuard Plus for Dogs 4-22")) =>
          28322349

        case name if (name.contains("ZoGuard Plus for Dogs 23-44")) =>
          30196793

        case name if (name.contains("ZoGuard Plus for Dogs 45-88")) =>
          30270887

        case _ =>
          0
      }
    }

    val possibleInsertOrderItem = shipment.insert.get match {
      case "TPP+Welcome Insert" => 
        OrderItem(
          orderItemId = 30198041
          quantity = 1,
        ) ::
        Nil
      case _ => Nil
    }

    val shipStationProducts = shipStationProductIds.map { id =>
      OrderItem(
        orderItemId = id
        quantity = 1,
      )
    }

    val 

    val newOrder = Order.create(
      orderNumber = s"${shipment.shipmentId}",
      orderDate = dateFormat.format(new Date()),
      shipByDate = Some(dateFormat.format(shipment.expectedShipDate.get)),
      orderStatus = "on_hold",
      billTo = billShipTo,
      shipTo = billShipTo,
      items = Some(shipStationProducts ++ possibleInsertOrderItem),
      customerNotes = petNamesProducts
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
