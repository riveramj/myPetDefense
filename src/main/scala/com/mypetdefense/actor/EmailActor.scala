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
  import mapper.{By}

import dispatch._
import org.joda.time._

import com.mypetdefense.model._
import com.mypetdefense.util._

import java.util.Date

import net.liftweb.util.Mailer
import Mailer._

sealed trait EmailActorMessage
case class SendWelcomeEmail(user: User) extends EmailActorMessage
case class SendInvoicePaymentFailedEmail(userEmail: String, amount: Double, nextPaymentAttempt: Option[DateTime]) extends EmailActorMessage
case class SendInvoicePaymentSucceededEmail(
  user: Box[User], 
  subscription: Subscription,
  taxPaid: String,
  amountPaid: String
) extends EmailActorMessage

trait WelcomeEmailHandling extends EmailHandlerChain {
  val welcomeEmailSubject = "Welcome to My Pet Defense!"
  val welcomeEmailTemplate = 
    Templates("emails-hidden" :: "welcome-email" :: Nil) openOr NodeSeq.Empty
  val baseEmailTemplate = 
    Templates("emails-hidden" :: "email-template" :: Nil) openOr NodeSeq.Empty
  
  val hostUrl = Paths.serverUrl

  addHandler {
    case SendWelcomeEmail(user) => 
      val transform = {
        "#first-name" #> user.firstName.get 
      }

      sendEmail(welcomeEmailSubject, user.email.get, transform(welcomeEmailTemplate))
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

      sendEmail(subject, userEmail, invoicePaymentFailedEmailTemplate)
  }
}

trait InvoicePaymentSucceededEmailHandling extends EmailHandlerChain {
  val invoicePaymentSucceededEmailTemplate =
    Templates("emails-hidden" :: "invoice-payment-succeeded-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendInvoicePaymentSucceededEmail(
      Full(user),
      subscription,
      taxPaid,
      amountPaid
    ) =>
      val subject = "My Pet Defense Receipt"
      val shipAddress = Address.find(By(Address.user, user), By(Address.addressType, AddressType.Shipping))
        val possibleBillAddress = Address.find(By(Address.user, user), By(Address.addressType, AddressType.Billing))

      val billAddress = {
        if (possibleBillAddress.isEmpty)
          shipAddress
        else
          possibleBillAddress
      }

      val dateFormatter = new SimpleDateFormat("MMM dd")

      val products = subscription.getProducts

      val transform = {
        "#ship-date" #> dateFormatter.format(new Date()) &
        "#parent-name" #> user.firstName &
        ".name" #> user.name &
        "#ship-address-1" #> shipAddress.map(_.street1.get) &
        "#ship-address-2" #> ClearNodesIf(shipAddress.map(_.street2.get).getOrElse("") == "") andThen
        "#ship-address-2-content" #> shipAddress.map(_.street2.get) &
        "#ship-city" #> shipAddress.map(_.city.get) &
        "#ship-state" #> shipAddress.map(_.state.get) &
        "#ship-zip" #> shipAddress.map(_.zip.get) &
        "#bill-address-1" #> billAddress.map(_.street1.get) &
        "#bill-address-2" #> ClearNodesIf(billAddress.map(_.street2.get).getOrElse("") == "") andThen
        "#bill-address-2-content" #> billAddress.map(_.street2.get) &
        "#bill-city" #> billAddress.map(_.city.get) &
        "#bill-state" #> billAddress.map(_.state.get) &
        "#bill-zip" #> billAddress.map(_.zip.get) &
        "#tax" #> ClearNodesIf(taxPaid.isEmpty) andThen
        ".ordered-product" #> products.map { product =>
          ".product *" #> s"${product.name.get}, ${product.size.get.toString} pounds" 
        } &
        "#tax #tax-due *" #> s"$$${taxPaid}" &
        "#total *" #> s"$$${amountPaid}"
      }
      
      sendEmail(subject, user.email.get, transform(invoicePaymentSucceededEmailTemplate))
  }
}

object EmailActor extends EmailActor
trait EmailActor extends EmailHandlerChain
                    with WelcomeEmailHandling
                    with InvoicePaymentFailedEmailHandling
                    with InvoicePaymentSucceededEmailHandling {

  val fromEmail = "mike.rivera@mypetdefense.com"
  val fromName = "My Pet Defense"

  def sendEmail(
    subject: String, 
    to: String, 
    message: NodeSeq
  ) {
    val emailTransform = {
      "#content *" #> message &
      "#logo [src]" #> (hostUrl + "/images/logo-name-white.png") &
      "#user-email" #> to
    }
    
    val body = emailTransform(baseEmailTemplate)

    Mailer.sendMail(
      From(fromEmail),
      Subject(subject),
      To(to),
      XHTMLMailBodyType(body)
    ) 
  }
}
