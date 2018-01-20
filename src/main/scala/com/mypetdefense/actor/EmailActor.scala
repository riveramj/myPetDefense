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
import com.mypetdefense.snippet._
import com.mypetdefense.util._

import java.util.Date

import net.liftweb.util.Mailer
import Mailer._

sealed trait EmailActorMessage
case class SendWelcomeEmail(user: User) extends EmailActorMessage
case class SendFeedbackEmail(user: User) extends EmailActorMessage
case class SendNewUserEmail(user: User) extends EmailActorMessage
case class SendPasswordResetEmail(user: User) extends EmailActorMessage
case class SendPasswordUpdatedEmail(user: User) extends EmailActorMessage
case class NewSaleEmail(user: User, petCount: Int, couponCode: String) extends EmailActorMessage
case class NewPetAddedEmail(user: User, pet: Pet) extends EmailActorMessage
case class PetRemovedEmail(user: User, pet: Pet) extends EmailActorMessage
case class BillingUpdatedEmail(user: User) extends EmailActorMessage
case class AccountCancelledEmail(user: User) extends EmailActorMessage
case class PaymentReceivedEmail(user: User, amount: Double) extends EmailActorMessage
case class PetAddedEmail(user: User, pet: Pet) extends EmailActorMessage
case class SendInvoicePaymentFailedEmail(
  user: User,
  amount: Double,
  nextPaymentAttempt: Option[DateTime]
) extends EmailActorMessage
case class SendInvoicePaymentSucceededEmail(
  user: Box[User], 
  subscription: Subscription,
  taxPaid: String,
  amountPaid: String
) extends EmailActorMessage
case class ContactUsEmail(
  name: String,
  email: String,
  message: String,
  sourcePage: String
) extends EmailActorMessage
case class TestimonialEmail(
  name: String,
  email: String,
  satisfactionRating: String,
  accuracyRating: String,
  convenientRating: String,
  recommendationRating: String,
  testimonial: String,
  comments: String
) extends EmailActorMessage
case class PictureEmail(
  name: String,
  email: String,
  dogName: String,
  instagram: String,
  dogLove: String
) extends EmailActorMessage

trait WelcomeEmailHandling extends EmailHandlerChain {
  val welcomeEmailSubject = "Welcome to My Pet Defense!"
  val welcomeEmailTemplate = 
    Templates("emails-hidden" :: "welcome-email" :: Nil) openOr NodeSeq.Empty
  
  val hostUrl = Paths.serverUrl

  addHandler {
    case SendWelcomeEmail(user) => 
      val transform = {
        "#first-name" #> user.firstName.get 
      }

      sendEmail(welcomeEmailSubject, user.email.get, transform(welcomeEmailTemplate))
  }
}

trait PetRemovedEmailHandling extends EmailHandlerChain {
  val petRemovedSubject = "Pet removed from account"
  val petRemovedTemplate = 
    Templates("emails-hidden" :: "internal-account-changes-email" :: Nil) openOr NodeSeq.Empty
  
  addHandler {
    case PetRemovedEmail(user, pet) =>

      val transform = {
        "#name" #> user.name &
        "#email" #> user.email.get &
        "#pet-name" #> pet.name.get &
        "#billing-updated" #> ClearNodes &
        "#pet-added" #> ClearNodes &
        "#account-cancelled" #> ClearNodes
      }

      sendEmail(petRemovedSubject, "help@mypetdefense.com", transform(petRemovedTemplate))
  }
}

trait NewPetAddedEmailHandling extends EmailHandlerChain {
  val newPetAddedSubject = "Pet removed from account"
  val newPetAddedTemplate = 
    Templates("emails-hidden" :: "internal-account-changes-email" :: Nil) openOr NodeSeq.Empty
  
  addHandler {
    case NewPetAddedEmail(user, pet) =>

      val transform = {
        "#name" #> user.name &
        "#email" #> user.email.get &
        "#pet-name" #> pet.name.get &
        "#billing-updated" #> ClearNodes &
        "#pet-removed" #> ClearNodes &
        "#account-cancelled" #> ClearNodes
      }

      sendEmail(newPetAddedSubject, "help@mypetdefense.com", transform(newPetAddedTemplate))
  }
}

trait BillingUpdatedHandling extends EmailHandlerChain {
  val billingUpdatedSubject = "Parent Billing Updated"
  val billingUpdatedTemplate = 
    Templates("emails-hidden" :: "internal-account-changes-email" :: Nil) openOr NodeSeq.Empty
  
  addHandler {
    case BillingUpdatedEmail(user) =>
      
      val transform = {
        "#name" #> user.name &
        "#email" #> user.email.get &
        "#pet-name-container" #> ClearNodes &
        "#pet-added" #> ClearNodes &
        "#pet-removed" #> ClearNodes &
        "#account-cancelled" #> ClearNodes
      }

      sendEmail(billingUpdatedSubject, "help@mypetdefense.com", transform(billingUpdatedTemplate))
  }
}

trait AccountCancelledHandling extends EmailHandlerChain {
  val subject = "Account Cancelled"
  val template = 
    Templates("emails-hidden" :: "internal-account-changes-email" :: Nil) openOr NodeSeq.Empty
  
  addHandler {
    case AccountCancelledEmail(user) =>
      
      val transform = {
        "#name" #> user.name &
        "#email" #> user.email.get &
        "#pet-name-container" #> ClearNodes &
        "#pet-added" #> ClearNodes &
        "#pet-removed" #> ClearNodes &
        "#billing-updated" #> ClearNodes
      }

      sendEmail(subject, "help@mypetdefense.com", transform(template))
  }
}

trait FeedbackEmailHandling extends EmailHandlerChain {
  val feedbackEmailSubject = "We Value Your Feedback - Free Month"
  val feedbackEmailTemplate = 
    Templates("emails-hidden" :: "feedback-email" :: Nil) openOr NodeSeq.Empty
  
  val feedbackLink = Paths.serverUrl + Paths.testimonial.loc.calcDefaultHref

  addHandler {
    case SendFeedbackEmail(user) => 
      val transform = {
        ".first-name" #> user.firstName.get &
        ".take-survy-link [href]" #> feedbackLink
      }

      sendEmail(feedbackEmailSubject, user.email.get, transform(feedbackEmailTemplate))
  }
}

trait SendNewUserEmailHandling extends EmailHandlerChain {
  val newUserSubject = "Your Account on My Pet Defense!"
  val newUserTemplate = 
    Templates("emails-hidden" :: "new-user-email" :: Nil) openOr NodeSeq.Empty
  
  addHandler {
    case SendNewUserEmail(user) =>
      val signupLink = Paths.serverUrl + Signup.menu.toLoc.calcHref(user)

      val transform = {
        "#first-name" #> user.firstName.get &
        "#signup [href]" #> signupLink
      }

      sendEmail(newUserSubject, user.email.get, transform(newUserTemplate))
  }
}

trait ResetPasswordHandling extends EmailHandlerChain {
  val resetSubject = "Reset your My Pet Defense password"
  val resetPasswordTemplate = 
    Templates("emails-hidden" :: "reset-password-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendPasswordResetEmail(userWithKey) => 
      val passwordResetLink = Paths.serverUrl + ResetPassword.menu.toLoc.calcHref(userWithKey)
      val transform = 
        "#reset-link [href]" #> passwordResetLink

      sendEmail(resetSubject, userWithKey.email.get, transform(resetPasswordTemplate))
  }
}

trait CompleteResetPasswordHandling extends EmailHandlerChain {
  val completeResetPasswordSubject = "My Pet Defense Password Changed"
  val completeResetPasswordTemplate = 
    Templates("emails-hidden" :: "complete-reset-password-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendPasswordUpdatedEmail(user) => 
      sendEmail(completeResetPasswordSubject, user.email.get, completeResetPasswordTemplate)
  }
}

trait InvoicePaymentFailedEmailHandling extends EmailHandlerChain {
  val invoicePaymentFailedEmailTemplate =
    Templates("emails-hidden" :: "invoice-payment-failed-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendInvoicePaymentFailedEmail(user, amount, nextPaymentAttempt) =>
      val finalAttempt = !nextPaymentAttempt.isDefined
      val subject = {
        if (finalAttempt)
          "Account Suspended"
        else
          "Problem Billing your Card"
      }

      val dateFormatter = new SimpleDateFormat("MMMM dd, yyyy")

      val transform = {
        "#card-problem [src]" #> (Paths.serverUrl + "/images/credit-card-problem@2x.png") &
        ".attempted-date *" #> dateFormatter.format(new Date()) &
        ".first-name" #> user.firstName.get &
        ".billing-url [href]" #> (Paths.serverUrl + ShippingBilling.menu.loc.calcDefaultHref) &
        ".will-bill-again" #> (nextPaymentAttempt.isDefined ? PassThru | ClearNodes) andThen
        ".will-not-bill-again" #> (nextPaymentAttempt.isDefined ? ClearNodes | PassThru) andThen
        ".bill-amount" #> ("$" + ("%1.2f" format amount)) &
        ".next-payment-attempt" #> nextPaymentAttempt.map { paymentAttemptDate =>
          dateFormatter.format(paymentAttemptDate.toDate)
        }
      }

      sendEmail(subject, user.email.get, transform(invoicePaymentFailedEmailTemplate))
  }
}

trait NewSaleEmailHandling extends EmailHandlerChain {
  addHandler {
    case NewSaleEmail(user, petCount, couponCode) =>
      val newSaleTemplate =
        Templates("emails-hidden" :: "sale-email" :: Nil) openOr NodeSeq.Empty
      
      val subject = "New Sale! Check dashboard!!"

      val transform = {
        "#name *" #> user.name &
        "#email *" #> user.email &
        "#pet-count *" #> petCount &
        "#coupon *" #> couponCode &
        ".amount" #> ClearNodes
      }

      sendEmail(subject, "sales@mypetdefense.com", transform(newSaleTemplate))
  }
}

trait ShipmentReadyEmailHandling extends EmailHandlerChain {
  addHandler {
    case PaymentReceivedEmail(user, amount) =>
      val paymentTemplate =
        Templates("emails-hidden" :: "sale-email" :: Nil) openOr NodeSeq.Empty
      
      val subject = "We got paid - time to ship product"
      
      val transform = {
        "#name *" #> user.name &
        "#email *" #> user.email &
        ".count" #> ClearNodes &
        ".coupon" #> ClearNodes &
        "#amount *" #> ("$" + ("%1.2f" format amount))
      }

      sendEmail(subject, "sales@mypetdefense.com", transform(paymentTemplate))
  }
}

trait ContactUsEmailHandling extends EmailHandlerChain {
  addHandler {
    case ContactUsEmail(name, email, message, sourcePage) =>
      val contactTemplate =
        Templates("emails-hidden" :: "contact-us-email" :: Nil) openOr NodeSeq.Empty
      
      val subject = "Landing Page Help Needed"
      
      val transform = {
        "#name *" #> name &
        "#email *" #> email &
        "#message *" #> message &
        "#source-page *" #> sourcePage
      }

      sendEmail(subject, "help@mypetdefense.com", transform(contactTemplate))
  }
}

trait TestimonialEmailHandling extends EmailHandlerChain {
  addHandler {
    case TestimonialEmail(name, email, satisfactionRating, accuracyRating, convenientRating, recommendationRating, testimonial, comments) =>
      val testimonialTemplate =
        Templates("emails-hidden" :: "testimonial-email" :: Nil) openOr NodeSeq.Empty
      
      val subject = "New Survey Result"
      
      val transform = {
        "#name *" #> name &
        "#email *" #> email &
        "#satisfaction-rating *" #> satisfactionRating &
        "#accuracy-rating *" #> accuracyRating &
        "#convenient-rating *" #> convenientRating &
        "#recommendation-rating *" #> recommendationRating &
        "#testimonial *" #> testimonial &
        "#comments *" #> comments
      }

      sendEmail(subject, "help@mypetdefense.com", transform(testimonialTemplate))
  }
}

trait PictureEmailHandling extends EmailHandlerChain {
  addHandler {
    case PictureEmail(name, email, dogName, instagram, dogLove) =>
      val testimonialTemplate =
        Templates("emails-hidden" :: "picture-email" :: Nil) openOr NodeSeq.Empty
      
      val subject = "New Picture Release"
      
      val transform = {
        "#name *" #> name &
        "#email *" #> email &
        "#dog-name *" #> dogName &
        "#instagram *" #> instagram &
        "#dog-love *" #> dogLove
      }

      sendEmail(subject, "mike.rivera@mypetdefense.com", transform(testimonialTemplate))
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
      val priceCode = subscription.priceCode.get

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
        "#tax" #> ClearNodesIf(taxPaid == "0") andThen
        ".ordered-product" #> products.map { product =>
          val price = {
            if (priceCode == null) {
              9.99D
            } else {
              Price.getPricesByCode(product, priceCode).map(_.price.get).openOr(0D)
            }
          }
          ".product *" #> s"${product.name.get}, ${product.size.get.toString} pounds" &
          ".amount-due *" #> s"$$${price}"
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
                    with FeedbackEmailHandling
                    with NewPetAddedEmailHandling
                    with PetRemovedEmailHandling
                    with BillingUpdatedHandling
                    with AccountCancelledHandling
                    with SendNewUserEmailHandling
                    with InvoicePaymentFailedEmailHandling
                    with InvoicePaymentSucceededEmailHandling 
                    with NewSaleEmailHandling 
                    with ResetPasswordHandling 
                    with CompleteResetPasswordHandling 
                    with ShipmentReadyEmailHandling 
                    with ContactUsEmailHandling
                    with TestimonialEmailHandling {

  val baseEmailTemplate = 
    Templates("emails-hidden" :: "email-template" :: Nil) openOr NodeSeq.Empty

  val fromEmail = "sales@mypetdefense.com"
  val fromName = "My Pet Defense"

  val envTag = {
    import net.liftweb.util.Props.RunModes._
    Props.mode match {
      case Development => "[LCL] "
      case Staging => "[DEMO] "
      case Pilot => "[DEV] "
      case _ => ""
    }
  }

  def sendEmail(
    subject: String, 
    to: String, 
    message: NodeSeq
  ) {
    val emailTransform = {
      "#content *" #> message &
      "#logo [src]" #> (hostUrl + "/images/logo/logo-name-white@2x.png") &
      "#user-email" #> to
    }
    
    val body = emailTransform(baseEmailTemplate)

    val envSubj = envTag + subject

    Mailer.sendMail(
      From(fromEmail),
      Subject(envSubj),
      To(to),
      XHTMLMailBodyType(body)
    ) 
  }
}
