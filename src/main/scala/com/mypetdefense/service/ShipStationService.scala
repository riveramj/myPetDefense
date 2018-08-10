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
import scala.util.{Failure => TryFail, _}
import scala.math.BigDecimal

import java.util.Date
import java.util.Base64
import java.nio.charset.StandardCharsets

object ShipStationService extends Loggable {
  val baseShipStationUrl = Props.get("shipstation.url") openOr ""
  val shipStationKey = Props.get("shipstation.key") openOr ""
  val shipStationSecret = Props.get("shipstation.secret") openOr ""

  val retryAttempts = 10

  val basicAuthentication = Base64.getEncoder.encodeToString(s"${shipStationKey}:${shipStationSecret}".getBytes(StandardCharsets.UTF_8))


  def getOrder(orderId: String) = {
    val getOrderUrl = url(s"${baseShipStationUrl}/orders/${orderId}")

    def getOrderResponse = {
      Http(getOrderUrl <:< Map("Authorization" -> s"Basic ${basicAuthentication}") OK as.String).either.map {
        case Left(throwable) =>
          logger.error(s"error from ShipStation: ${throwable}")
          Failure("Error occured while talking to shipstation.", Full(throwable), Empty)
        case Right(possibleOrder) =>
          Full(possibleOrder)
      }
    }

    def rawOrder(attemptsLeft: Int): Box[String] = {
      Try(Await.result(getOrderResponse, 1 seconds)) match {
        case Success(orderResponse) => 
          orderResponse
        case TryFail(throwable: Throwable) =>
          if (attemptsLeft > 0)
            rawOrder(attemptsLeft - 1)
          else {
            logger.error(s"Timeout occured while talking to shipStation for get order with ${throwable}")
            Failure("Timeout occured while talking to shipStation for get order.", Full(throwable), Empty)
          }

      }
    }

    val order = rawOrder(retryAttempts)

    order
  }

  def createOrder(orderDetails: ShipStationOrder) = {
    val creatOrderUrl = url(s"${baseShipStationUrl}/orders/createorder")

    def getOrderResponse = {
      Http(creatOrderUrl << Map(
        ) <:< Map("Authorization" -> s"Basic ${basicAuthentication}") OK as.String).either.map {
        case Left(throwable) =>
          logger.error(s"error from ShipStation: ${throwable}")
          Failure("Error occured while talking to shipstation.", Full(throwable), Empty)
        case Right(possibleOrder) =>
          Full(possibleOrder)
      }
    }

    def rawOrder(attemptsLeft: Int): Box[String] = {
      Try(Await.result(getOrderResponse, 1 seconds)) match {
        case Success(orderResponse) => 
          orderResponse
        case TryFail(throwable: Throwable) =>
          if (attemptsLeft > 0)
            rawOrder(attemptsLeft - 1)
          else {
            logger.error(s"Timeout occured while talking to shipStation for get order with ${throwable}")
            Failure("Timeout occured while talking to shipStation for get order.", Full(throwable), Empty)
          }

      }
    }

    val order = rawOrder(retryAttempts)

    order
    
  }
}
