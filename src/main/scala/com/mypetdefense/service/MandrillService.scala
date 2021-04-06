package com.mypetdefense.service

import dispatch.Defaults._
import dispatch._
import net.liftweb.common._
import net.liftweb.json.JsonDSL._
import net.liftweb.json._
import net.liftweb.util.Props
import org.asynchttpclient.Response

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.parsing.json.JSON
import scala.util.{Failure => TryFail, _}

object MandrillService extends Loggable {
  implicit val formats: Formats = DefaultFormats

  val sendMessageWithTemplateUrl: Req = url("https://mandrillapp.com/api/1.0/messages/send-template").secure
  val pingUrl: Req = url("https://mandrillapp.com/api/1.0/users/ping").secure
  val authKey: String = Props.get("mail.test.auth.key") openOr ""
  val retryAttempts = 10


  def sendTemplateEmail(
    subject: String,
    toEmail: String,
    templateName: String,
    emailVars: Map[String, String],
    fromEmail: String
  ) = {
    def emailResponse(messageContents: JValue): Future[Box[String]] = {
      val bar = parse("{}")
      val fooRaw = Map(
        "key" -> "MagByJ-S9IT6C7vUa2aYeQ",
        "template_name" -> templateName,
        "template_content" -> compactRender(Nil),
        "message" -> compactRender(List(bar))
      )

      println(fooRaw)
      val fooString = Serialization.write(fooRaw.toString)
      println(fooString)

      Http
        .default(
          sendMessageWithTemplateUrl
              <<
            "{\"template_name\": \"reset password temp\",\"key\": \"MagByJ-S9IT6C7vUa2aYeQ\",\"template_content\": [],\"message\": {\"to\": [{\"email\": \"rivera.mj@gmail.com\"}],\"global_merge_vars\": [{\"name\": \"name\",\"content\": \"mike\"}]}}" <:< Map("Content-Type" -> "application/json") > successAsStringOrThrowError
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
    }

    @tailrec
    def sendEmail(messageContents: JValue, attemptsLeft: Int): Box[String] =
      Try(Await.result(emailResponse(messageContents), 1 seconds)) match {
        case Success(response) =>
          response
        case TryFail(throwable: Throwable) =>
          if(attemptsLeft > 0)
            sendEmail(messageContents, attemptsLeft - 1)
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

    val messageJson: JValue =
      List(("message" ->
        ("subject" -> subject) ~
        ("to" -> List(
          ("email" -> toEmail)
        )) ~
        ("global_merge_vars" ->
          emailVars.map { case (name, value) =>
            ("name" -> name) ~
            ("content" -> value)
          }
        )
      ))

    val messageContents = compactRender(messageJson)

    val email = sendEmail(messageJson, retryAttempts)

    email
  }

  private val successAsStringOrThrowError: Response => String =
    resp =>
      if(resp.getStatusCode / 100 == 2) as.String(resp)
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
