package com.mypetdefense.util

import net.liftweb.actor._
import net.liftweb.common._

import scala.xml.NodeSeq

trait HandlerChain extends LiftActor with Loggable {
  override lazy val messageHandler: PartialFunction[Any, Unit] =
    handlers.foldRight(defaultHandler)(_ orElse _)
  private val defaultHandler: PartialFunction[Any, Unit] = {
    case somethingUnexpected =>
      logger.warn("Got a message I didn't expect: " + somethingUnexpected)
  }
  private var handlers: List[PartialFunction[Any, Unit]] = Nil

  protected def addHandler(pf: PartialFunction[Any, Unit]): Unit =
    handlers = handlers :+ pf
}

trait EmailHandlerChain extends HandlerChain {
  def sendEmail(
      subject: String,
      to: String,
      body: NodeSeq,
      fromEmail: String = "sales@mypetdefense.com"
  ): Unit

  def sendTemplateEmail(
     subject: String,
     to: String,
     templateName: String,
     emailVars: Map[String, String],
     fromEmail: String = "sales@mypetdefense.com"
   ): Unit
}
