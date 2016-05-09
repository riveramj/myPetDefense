package com.mypetdefense.actor

import scala.xml.NodeSeq
import scala.util._

import java.text.SimpleDateFormat

import net.liftweb._
  import common._
  import actor._
  import util.Helpers._
  import util._
  import http._

import dispatch._
import org.joda.time._

import com.mypetdefense.model._
import com.mypetdefense.util._

import net.liftweb.util.Mailer
import Mailer._

sealed trait EmailActorMessage
case class SendWelcomeEmail(userEmail: String) extends EmailActorMessage
case class SendInvoicePaymentFailedEmail(userEmail: String, amount: Double, nextPaymentAttempt: Option[DateTime]) extends EmailActorMessage
case class SendInvoicePaymentSucceededEmail(user: Box[User]) extends EmailActorMessage

trait WelcomeEmailHandling extends EmailHandlerChain {
  val welcomeEmailSubject = "Welcome to My Pet Defense!"
  val welcomeEmailTemplate = 
    Templates("emails-hidden" :: "welcome-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendWelcomeEmail(userEmail) =>
      sendEmail(welcomeEmailSubject, userEmail, "Welcome!")
  }
}

trait InvoicePaymentFailedEmailHandling extends EmailHandlerChain {
  val invoicePaymentFailedEmailTemplate =
    Templates("emails-hidden" :: "invoice-payment-failed-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendInvoicePaymentFailedEmail(userEmail, amount, nextPaymentAttempt) =>
      val subject = "Problem Billing your Credit Card"
      val dateFormatter = new SimpleDateFormat("MMM dd")

      val invoicePaymentFailedMessage = (
        ".will-bill-again" #> (nextPaymentAttempt.isDefined ? PassThru | ClearNodes) andThen
        ".will-not-bill-again" #> (nextPaymentAttempt.isDefined ? ClearNodes | PassThru) andThen
        ".bill-amount" #> ("$" + ("%1.2f" format amount)) &
        ".next-payment-attempt" #> nextPaymentAttempt.map { paymentAttemptDate =>
          dateFormatter.format(paymentAttemptDate.toDate)
        }
      ).apply(invoicePaymentFailedEmailTemplate)

      sendEmail(subject, userEmail, "Payment Failed!")
  }
}

trait InvoicePaymentSucceededEmailHandling extends EmailHandlerChain {
  val invoicePaymentSucceededEmailTemplate =
    Templates("emails-hidden" :: "invoice-payment-succeeded-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendInvoicePaymentSucceededEmail(user) =>
      val subject = "My Pet Defense Receipt"
      sendEmail(subject, user.map(_.email.get).openOr(""), "Bill paid & product in mail!")
  }
}

object EmailActor extends EmailActor
trait EmailActor extends EmailHandlerChain
                    with WelcomeEmailHandling
                    with InvoicePaymentFailedEmailHandling
                    with InvoicePaymentSucceededEmailHandling {

  val fromEmail = "mike.rivera@mypetdefense.com"
  val fromName = "My Pet Defense"

  def sendEmail(subject: String, to: String, body: String) {
    Mailer.sendMail(
      From(fromEmail),
      Subject(subject),
      To(to),
      PlainMailBodyType(body)
    ) 
  }
}
