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

import java.util.Date

object TaxJarService extends Loggable {
  val calculateTaxUrl = url("https://api.taxjar.com/v2/taxes")
  val createOrderTaxUrl = url("https://api.taxjar.com/v2/transactions/orders")

  val authKey = Props.get("taxjar.api.key") openOr ""

  val retryAttempts = 10
  
  def findTaxAmoutAndRate(city: String, state: String, zip: String, amount: Double): (Double, Double) = {
    def taxResponse = {
      Http(calculateTaxUrl.secure << Map(
        "to_country" -> "US",
        "to_zip" -> zip,
        "to_state" -> state,
        "to_city" -> city,
        "amount" -> amount.toString,
        "shipping" -> "0"
      ) <:< Map("Authorization" -> s"Bearer ${authKey}") OK as.String).either.map {
        case Left(throwable) =>
          logger.error(s"taxjar error: ${throwable}")
          Failure("Error occured while talking to taxJar.", Full(throwable), Empty)
        case Right(possibleTaxReseponse) =>
          Full(possibleTaxReseponse)
      }
    }

    def rawTax(attemptsLeft: Int): Box[String] = {
      Try(Await.result(taxResponse, 1 seconds)) match {
        case Success(taxResponse) => 
          taxResponse
        case TryFail(throwable: Throwable) =>
          if (attemptsLeft > 0)
            rawTax(attemptsLeft - 1)
          else {
            logger.error(s"Timeout occured while talking to taxJar for taxt calc with ${throwable}")
            Failure("Timeout occured while talking to taxJar for taxt calc.", Full(throwable), Empty)
          }

      }
    }

    val parsedTax = parse(rawTax(retryAttempts).openOr("")) 

    (for {
      JField("amount_to_collect", JDouble(taxDue)) <- parsedTax
      JField("rate", JDouble(taxRate)) <- parsedTax
      amount <- tryo(taxDue.toDouble).toList
      rate <- tryo(taxRate.toDouble).toList
    } yield {
      (amount, rate * 100)
    }).headOption.getOrElse((0D, 0D))
  }

  def processTaxesCharged(orderIdentifier: String, city: String, state: String, zip: String, amount: String, rawTax: String) = {
    if (rawTax != "0") {
      val subtotal = tryo(amount.toDouble/100.0).openOr(0D)
      val tax = tryo(rawTax.toDouble/100.0).openOr(0D)

      createTaxOrder(
        orderIdentifier,
        city,
        state,
        zip,
        subtotal.toString(),
        tax.toString(),
        new Date().toString
      )
    }
  }

  def createTaxOrder(orderIdentifier: String, city: String, state: String, zip: String, amount: String, tax: String, date: String) = {
    def orderResponse = {
      Http(createOrderTaxUrl.secure << Map(
        "transaction_id" -> orderIdentifier,
        "transaction_date" -> date,
        "to_country" -> "US",
        "to_zip" -> zip,
        "to_state" -> state,
        "to_city" -> city,
        "amount" -> amount,
        "shipping" -> "0",
        "sales_tax" -> tax
      ) <:< Map("Authorization" -> s"Bearer ${authKey}") OK as.String).either.map {
        case Left(throwable) =>
          logger.error(s"taxjar error: ${throwable}")
          Failure("Error occured while talking to taxJar.", Full(throwable), Empty)
        case Right(possibleOrderReseponse) =>
          Full(possibleOrderReseponse)
      }
    }

    def rawOrder(attemptsLeft: Int): Box[String] = {
      Try(Await.result(orderResponse, 1 seconds)) match {
        case Success(response) => 
          response
        case TryFail(throwable: Throwable) =>
          if (attemptsLeft > 0)
            rawOrder(attemptsLeft - 1)
          else {
            logger.error(s"Timeout occured while talking to taxJar for orderCreate with ${throwable}")
            Failure("Timeout occured while talking to taxJar for orderCreate.", Full(throwable), Empty)
          }
      }
    }

    val order = rawOrder(retryAttempts)

    println(order)
    order
  }
}
