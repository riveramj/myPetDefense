package com.mypetdefense.snippet

import net.liftweb._
  import common._
  import mapper._
  import http._
    import LiftRules._
    import rest._
    import js._
      import JE._
      import JsExp._
  import util._
    import Helpers.tryo
  import json._
    import Extraction._
    import JsonDSL._

import scala.util.{Failure => TryFail, Success => TrySuccess, _}
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

import dispatch.{Req => DispatchReq, _}, Defaults._
import scala.concurrent.duration._
import scala.language.postfixOps

import scala.collection.concurrent.TrieMap
import scala.math.BigDecimal
import java.util.Date

object PromikaAPI extends RestHelper with Loggable {
  val templateEmailUrl = url("https://api.postmarkapp.com/email/withTemplate")

  serve {
    case req @ Req("api" :: "promika" :: "email" :: Nil, _, GetRequest) => {

      for {
        email <- req.param("email").filter(_.trim.nonEmpty) ?~! "Email was not specified." ~> 400
      } yield {
        val body = s"""{
          'TemplateId': 5333842,
          'TemplateAlias': 'string',
          'TemplateModel': {},
          'From': 'NoReply@promikallcresources.com',
          'To': '${email}'
        }"""

        val emailSend = Http(templateEmailUrl.POST.setBody(body) <:< Map(
          "content-type" -> "application/json",
          "accept" -> "application/json",
          "X-Postmark-Server-Token" -> "66f05a8f-9a86-4f1a-aa66-72e22eee081e"
        )).either.map {
          case Left(throwable) =>
            logger.error(s"postmark error: ${throwable}")
            Failure("Error occured while talking to postmark.", Full(throwable), Empty)

          case Right(sentEmail) =>
            Full(sentEmail)
        }

        Try(Await.result(emailSend, 1 seconds)) match {
          case TrySuccess(response) => 
            response
          case TryFail(throwable: Throwable) =>
            //if (attemptsLeft > 0)
            //rawTax(attemptsLeft - 1)
            //else {
            logger.error(s"Timeout occured while talking to taxJar for taxt calc with ${throwable}")
            Failure("Timeout occured while talking to taxJar for taxt calc.", Full(throwable), Empty)
            //}
        }

        OkResponse()
      }
    }
  }
}
