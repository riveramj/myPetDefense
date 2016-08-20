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

import java.util.Date

object TaxJarService extends Loggable {
  val calculateTaxUrl = url("https://api.taxjar.com/v2/taxes")
  val createOrderTaxUrl = url("https://api.taxjar.com/v2/transactions/orders")

  val authKey = Props.get("taxjar.api.key") openOr ""
  
  def findTaxAmoutAndRate(city: String, state: String, zip: String, amount: Double): (Double, Double) = {
    val taxResponse = Http(calculateTaxUrl.secure << Map(
      "to_country" -> "US",
      "to_zip" -> zip,
      "to_state" -> state,
      "to_city" -> city,
      "amount" -> amount.toString,
      "shipping" -> "0"
    ) <:< Map("Authorization" -> s"Bearer ${authKey}") OK as.String).either.map {
      case Left(throwable) =>
        Failure("Error occured while talking to taxJar.", Full(throwable), Empty)

      case Right(possibleTaxReseponse) =>
        Full(possibleTaxReseponse)
    }

    val rawTax = Await.result(taxResponse, 3 seconds)
    val parsedTax = parse(rawTax.openOr("")) 

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
    val orderResponse = Http(createOrderTaxUrl.secure << Map(
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
        Failure("Error occured while talking to taxJar.", Full(throwable), Empty)

      case Right(possibleOrderReseponse) =>
        Full(possibleOrderReseponse)
    }

    val rawOrder = Await.result(orderResponse, 3 seconds)

    println(rawOrder)
    rawOrder
  }
}
