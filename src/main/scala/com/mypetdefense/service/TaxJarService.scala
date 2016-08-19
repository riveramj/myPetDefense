package com.mypetdefense.service

import net.liftweb._ 
  import common._
  import util.Helpers.tryo
  import json._

import dispatch._, Defaults._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

object TaxJarService extends Loggable {
  val calculateTaxUrl = url("https://api.taxjar.com/v2/taxes")
  def calculateTaxPost = calculateTaxUrl.POST

  def findTaxAmout(state: String, zip: String, amount: String): Double = {
    val taxResponse = Http(url("https://api.taxjar.com/v2/taxes").secure << Map(
      "to_country" -> "US",
      "to_zip" -> zip,
      "to_state" -> state,
      "amount" -> "9.99",
      "shipping" -> "0"
    ) <:< Map("Authorization" -> "Bearer a898d7a70fb3f3d948b7aac38779e42d") OK as.String).either.map {
      case Left(throwable) =>
        Failure("Error occured while talking to taxJar.", Full(throwable), Empty)

      case Right(possibleTaxReseponse) =>
        Full(possibleTaxReseponse)
    }

    val rawTax = Await.result(taxResponse, 1 seconds)
    val possibleTax = parse(rawTax.openOr("")) 

    (for {
      JField("amount_to_collect", JDouble(taxDue)) <- possibleTax
      amount <- tryo(taxDue.toDouble).toList
    } yield {
      amount
    }).headOption.getOrElse(0D)
  }
}
