package com.mypetdefense.service

import dispatch.Defaults._
import dispatch._
import net.liftweb.common._
import net.liftweb.json.Extraction.decompose
import net.liftweb.json.JsonDSL._
import net.liftweb.json._
import net.liftweb.util.Props
import org.asynchttpclient.Response

import scala.language.postfixOps

object MandrillService extends Loggable {
  implicit val formats: Formats = DefaultFormats

  private val endpointHost = "mandrillapp.com"
  private val endpointVersion = "1.0"
  private val authKey: String = Props.get("mail.test.auth.key") openOr ""

  case class MandrillTo(email: String, name: Option[String] = None, `type`: String = "to")

  case class MandrillMessage(
    subject: String,
    from_email: String,
    to: List[MandrillTo],
    from_name: Option[String] = None,
    html: Option[String] = None,
    text: Option[String] = None
  )

  trait MandrillApiCall {
    def uri(requestBuilder: dispatch.Req): dispatch.Req
  }

  case class SendMandrillMessage(message: MandrillMessage, async: Boolean = false) extends MandrillApiCall {
    def uri(requestBuilder: dispatch.Req): Req = requestBuilder / "messages" / "send.json"
  }

  case class SendTemplateMandrillMessage(message: MandrillMessage, template_name: String, template_content: List[Map[String, String]], async: Boolean = false) extends MandrillApiCall {
    def uri(requestBuilder: dispatch.Req): Req = requestBuilder / "messages" / "send-template.json"
  }

  case class CodeResponse(code: Int)
  protected object AsCodeResponse extends (Response => CodeResponse) {
    def apply(r:Response): CodeResponse = {
      CodeResponse(
        r.getStatusCode()
      )
    }
  }

  def sendTemplateEmail(apiCall: MandrillApiCall) = {
    val postJson = decompose(apiCall) match {
      case obj:JObject =>
        obj ~
          ("key" -> authKey)

      case _ => JObject(Nil)
    }

    val requestBody = compactRender(postJson)
    val request = (apiCall.uri(host(endpointHost) / "api" / endpointVersion)).secure <<
      requestBody

    val response = Http.default(request > AsCodeResponse).either

    response() match {
      case Right(CodeResponse(200)) => // All good.
      case Right(CodeResponse(code)) => logger.error("Mandrill returned code: " + code)
      case Left(dispatchError) => logger.error("Dispatch error: " + dispatchError)
    }
  }
}
