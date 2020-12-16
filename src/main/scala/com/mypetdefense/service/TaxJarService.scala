package com.mypetdefense.service

import java.util.Date

import dispatch.Defaults._
import dispatch._
import net.liftweb.common._
import net.liftweb.json._
import net.liftweb.util.Helpers.tryo
import net.liftweb.util.Props
import org.asynchttpclient.Response

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure => TryFail, _}

object TaxJarService extends Loggable {
  val calculateTaxUrl: Req   = url("https://api.taxjar.com/v2/taxes").secure
  val createOrderTaxUrl: Req = url("https://api.taxjar.com/v2/transactions/orders").secure

  val authKey: String = Props.get("taxjar.api.key") openOr ""

  val retryAttempts = 10

  def calculateTaxRate(city: String, state: String, zip: String): Double = {
    findTaxAmountAndRate(city, state, zip, 0d)._2
  }

  def findTaxAmountAndRate(
      city: String,
      state: String,
      zip: String,
      amount: BigDecimal
  ): (Double, Double) = {
    def taxResponse: Future[Box[String]] =
      Http
        .default(
          calculateTaxUrl << Map(
            "to_country"            -> "US",
            "to_zip"                -> zip,
            "to_state"              -> state,
            "to_city"               -> city,
            "amount"                -> amount.toString,
            "shipping"              -> "0"
          ) <:< Map("Authorization" -> s"Bearer $authKey") > successAsStringOrThrowError
        )
        .either
        .map {
          case Left(e: UnexpectedResponseException) =>
            logger.error(s"[findTaxAmountAndRate > taxResponse] ${e.prettyPrint}", e)
            Failure("Unexpected response while talking to taxJar.", Full(e), Empty)
          case Left(throwable) =>
            logger.error(s"[findTaxAmountAndRate > taxResponse] taxjar error", throwable)
            Failure("Error occurred while talking to taxJar.", Full(throwable), Empty)
          case Right(possibleTaxResponse) =>
            Full(possibleTaxResponse)
        }

    @tailrec
    def rawTax(attemptsLeft: Int): Box[String] =
      Try(Await.result(taxResponse, 1 seconds)) match {
        case Success(taxResponse) =>
          taxResponse
        case TryFail(throwable: Throwable) =>
          if (attemptsLeft > 0)
            rawTax(attemptsLeft - 1)
          else {
            logger.error(
              s"[findTaxAmountAndRate > rawTax] Timeout occurred while talking to taxJar for tax calc.",
              throwable
            )
            Failure(
              "Timeout occurred while talking to taxJar for tax calc.",
              Full(throwable),
              Empty
            )
          }

      }

    val parsedTax = parse(rawTax(retryAttempts).openOr(""))

    (for {
      JObject(tax)                                 <- parsedTax
      JField("amount_to_collect", JDouble(taxDue)) <- tax
      JField("rate", JDouble(taxRate))             <- tax
      amount                                       <- tryo(taxDue.toDouble).toList
      rate                                         <- tryo(taxRate.toDouble).toList
    } yield {
      val normalizedRate = tryo(
        BigDecimal(rate * 100).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
      ).openOr(0d)

      (amount, normalizedRate)
    }).headOption.getOrElse((0d, 0d))
  }

  def processTaxesCharged(
      orderIdentifier: String,
      city: String,
      state: String,
      zip: String,
      subtotal: String,
      tax: String
  ): Any = {
    println("in taxes charged")
    println(s"tax collected is ${tax}")
    println(s"orderIdentifier is ${orderIdentifier}")
    println(s"city is ${city}")
    println(s"state is ${state}")
    println(s"zip is ${zip}")
    println(s"subtotal is ${subtotal}")
    println("===================")
    if (tax != "0") {
      println("in if")
      createTaxOrder(
        orderIdentifier,
        city,
        state,
        zip,
        subtotal,
        tax,
        new Date().toString
      )
    }
  }

  def createTaxOrder(
      orderIdentifier: String,
      city: String,
      state: String,
      zip: String,
      amount: String,
      tax: String,
      date: String
  ): Box[String] = {
    def orderResponse: Future[Box[String]] =
      Http
        .default(
          createOrderTaxUrl << Map(
            "transaction_id"        -> orderIdentifier,
            "transaction_date"      -> date,
            "to_country"            -> "US",
            "to_zip"                -> zip,
            "to_state"              -> state,
            "to_city"               -> city,
            "amount"                -> amount,
            "shipping"              -> "0",
            "sales_tax"             -> tax
          ) <:< Map("Authorization" -> s"Bearer $authKey") > successAsStringOrThrowError
        )
        .either
        .map {
          case Left(e: UnexpectedResponseException) =>
            logger.error(s"[createTaxOrder > orderResponse] ${e.prettyPrint}", e)
            Failure("Unexpected response while talking to taxJar.", Full(e), Empty)
          case Left(throwable) =>
            logger.error(s"[createTaxOrder > orderResponse] taxjar error.", throwable)
            Failure("Error occurred while talking to taxJar.", Full(throwable), Empty)
          case Right(possibleOrderResponse) =>
            Full(possibleOrderResponse)
        }

    @tailrec
    def rawOrder(attemptsLeft: Int): Box[String] =
      Try(Await.result(orderResponse, 1 seconds)) match {
        case Success(response) =>
          response
        case TryFail(throwable: Throwable) =>
          if (attemptsLeft > 0)
            rawOrder(attemptsLeft - 1)
          else {
            logger.error(
              s"[createTaxOrder > raw order] Timeout occurred while talking to taxJar for orderCreate.",
              throwable
            )
            Failure(
              "Timeout occurred while talking to taxJar for orderCreate.",
              Full(throwable),
              Empty
            )
          }
      }

    println("before raw order")
    val order = rawOrder(retryAttempts)
    println("after raw order")

    order
  }

  private val successAsStringOrThrowError: Response => String =
    resp =>
      if (resp.getStatusCode / 100 == 2) as.String(resp)
      else throw UnexpectedResponseException(resp)

  private final case class UnexpectedResponseException(resp: Response)
      extends RuntimeException(s"Unexpected response: ${resp.getStatusCode}") {

    def prettyPrint: String =
      s"""Unexpected response:
         |  * status code: ${resp.getStatusCode}
         |  * headers: $formatHeaders
         |  * body: ${resp.getResponseBody}
         |""".stripMargin

    private def formatHeaders: String =
      resp.getHeaders.iteratorAsString.asScala
        .map(entry => s"    * ${entry.getKey}: ${entry.getValue}")
        .mkString("\n", "\n", "")
  }
}
