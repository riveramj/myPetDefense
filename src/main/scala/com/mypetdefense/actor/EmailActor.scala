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

import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import java.time.{ZoneId, LocalDateTime}
import java.time.format.DateTimeFormatter

import net.liftweb.util.Mailer
import Mailer._

sealed trait EmailActorMessage
case class SendWelcomeEmail(user: User) extends EmailActorMessage
case class SendFeedbackEmail(user: User) extends EmailActorMessage
case class SendNewAdminEmail(user: User) extends EmailActorMessage
case class SendNewAgentEmail(user: User) extends EmailActorMessage
case class SendNewUserEmail(user: User) extends EmailActorMessage
case class SendPasswordResetEmail(user: User) extends EmailActorMessage
case class SendPasswordUpdatedEmail(user: User) extends EmailActorMessage
case class NewSaleEmail(user: User, petCount: Int, couponCode: String) extends EmailActorMessage
case class NewPetAddedEmail(user: User, pet: Pet) extends EmailActorMessage
case class PetRemovedEmail(user: User, pet: Pet) extends EmailActorMessage
case class BillingUpdatedEmail(user: User) extends EmailActorMessage
case class AccountCancelledEmail(user: User) extends EmailActorMessage
case class ParentCancelledAccountEmail(user: User) extends EmailActorMessage
case class ParentPauseSubscriptionEmail(user: User, subscription: Subscription) extends EmailActorMessage
case class ParentResumeSubscriptionEmail(user: User, subscription: Subscription) extends EmailActorMessage
case class PaymentReceivedEmail(user: User, amount: Double) extends EmailActorMessage
case class SendAPIErrorEmail(emailBody: String) extends EmailActorMessage
case class SendTppApiJsonEmail(emailBody: String) extends EmailActorMessage
case class NotifyParentGrowthRate(pet: Pet, newProduct: String, user: User) extends EmailActorMessage
case class TreatReceiptEmail(order: TreatOrder) extends EmailActorMessage
case class TreatShippedEmail(order: TreatOrder) extends EmailActorMessage
case class SendShipmentRefundedEmail(parent: Box[User], shipment: Shipment) extends EmailActorMessage
case class DailySalesEmail(
  agentNameAndCount: List[(String, Int)],
  monthAgentNameAndCount: List[(String, Int)],
  dailySalesByAgency: List[(String, Int)],
  monthlySalesByAgency: List[(String, Int)],
  email: String
) extends EmailActorMessage
case class InternalDailyEmail(
  newShipmentCount: Int,
  paidShipmentCount: Int,
  grossSales: Double,
  cancelsCount: Int,
  email: String
) extends EmailActorMessage
case class SendInvoicePaymentFailedEmail(
  user: User,
  amount: Double,
  nextPaymentAttempt: Option[DateTime]
) extends EmailActorMessage
case class SendInvoicePaymentSucceededEmail(
  user: Box[User], 
  subscription: Box[Subscription],
  taxPaid: String,
  amountPaid: String,
  possibleTrackingNumber: String
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
case class Send5kEmail(
  name: String,
  email: String,
  dogName: String
) extends EmailActorMessage
case class Send6MonthSaleReceipt(
  user: User,
  pets: List[Pet],
  subtotal: Double,
  tax: Double
)

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
  val newPetAddedSubject = "Pet added to account"
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

trait ParentCancelledAccountHandling extends EmailHandlerChain {
  addHandler {
    case ParentCancelledAccountEmail(user) =>

      val subject = "Subscription Cancelled"
      val template = 
        Templates("emails-hidden" :: "parent-account-cancelled-email" :: Nil) openOr NodeSeq.Empty

      val transform = {
        ".name *" #> user.firstName.get &
        "#email *" #> user.email.get
      }

      sendEmail(subject, user.email.get, transform(template))
  }
}

trait ParentPauseSubscriptionHandling extends EmailHandlerChain {
  addHandler {
    case ParentPauseSubscriptionEmail(user, subscription) =>

      val dateFormatter = new SimpleDateFormat("MMMM dd, yyyy")
      val subject = "Subscription Paused"
      val template = 
        Templates("emails-hidden" :: "parent-subscription-paused-email" :: Nil) openOr NodeSeq.Empty

      val transform = {
        ".name *" #> user.firstName.get &
        "#email *" #> user.email.get &
        ".next-ship-date *" #> dateFormatter.format(subscription.nextShipDate.get)
      }

      sendEmail(subject, user.email.get, transform(template))
  }
}

trait ParentResumeSubscriptionHandling extends EmailHandlerChain {
  addHandler {
    case ParentResumeSubscriptionEmail(user, subscription) =>

      val dateFormatter = new SimpleDateFormat("MMMM dd, yyyy")
      val subject = "Subscription Active"
      val template = 
        Templates("emails-hidden" :: "parent-subscription-resumed-email" :: Nil) openOr NodeSeq.Empty

      val transform = {
        ".name *" #> user.firstName.get &
        "#email *" #> user.email.get &
        ".next-ship-date *" #> dateFormatter.format(subscription.nextShipDate.get)
      }

      sendEmail(subject, user.email.get, transform(template))
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

trait SendNewAdminEmailHandling extends EmailHandlerChain {
  addHandler {
    case SendNewAdminEmail(user) =>
      val subject = "Your Admin Account on My Pet Defense"
      val template = 
        Templates("emails-hidden" :: "new-user-email" :: Nil) openOr NodeSeq.Empty

      val signupLink = Paths.serverUrl + Signup.menu.toLoc.calcHref(user)

      val transform = {
        ".customer" #> ClearNodes &
        ".agent" #> ClearNodes &
        "#first-name" #> user.firstName.get &
        "#signup [href]" #> signupLink
      }

      sendEmail(subject, user.email.get, transform(template))
  }
}

trait SendNewAgentEmailHandling extends EmailHandlerChain {
  addHandler {
    case SendNewAgentEmail(user) =>
      val subject = "Your Account on My Pet Defense"
      val template = 
        Templates("emails-hidden" :: "new-user-email" :: Nil) openOr NodeSeq.Empty

      val signupLink = Paths.serverUrl + Signup.menu.toLoc.calcHref(user)

      val transform = {
        ".customer" #> ClearNodes &
        ".admin" #> ClearNodes &
        "#first-name" #> user.firstName.get &
        "#signup [href]" #> signupLink
      }

      sendEmail(subject, user.email.get, transform(template))
  }
}

trait SendNewUserEmailHandling extends EmailHandlerChain {
  addHandler {
    case SendNewUserEmail(user) =>
      val subject = "Your Account on My Pet Defense!"
      val template = 
        Templates("emails-hidden" :: "new-user-email" :: Nil) openOr NodeSeq.Empty

      val signupLink = Paths.serverUrl + Signup.menu.toLoc.calcHref(user)

      val transform = {
        ".admin" #> ClearNodes &
        ".agent" #> ClearNodes &
        "#first-name" #> user.firstName.get &
        "#signup [href]" #> signupLink
      }

      sendEmail(subject, user.email.get, transform(template))
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

      sendEmail(subject, "mike.rivera@mypetdefense.com", transform(paymentTemplate))
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

trait Send5kEmailHandling extends EmailHandlerChain {
  addHandler {
    case Send5kEmail(name, email, dogName) =>
      val template =
        Templates("emails-hidden" :: "cold5k-picture-email" :: Nil) openOr NodeSeq.Empty
      
      val subject = "Your Valentine Pictures are Ready"
      val hostUrl = Paths.serverUrl
      
      val transform = {
        "#first-name *" #> name &
        "#email *" #> email &
        ".view-pictures [href]" #> (s"${hostUrl}/valentine/${dogName}")
      }

      sendEmail(subject, email, transform(template))
  }
}

trait SendAPIErrorEmailHandling extends EmailHandlerChain {
  addHandler {
    case SendAPIErrorEmail(emailBody) =>
      val template =
        Templates("emails-hidden" :: "api-error-email" :: Nil) openOr NodeSeq.Empty
      
      val subject = "API Error - Manual Work Needed"
      val hostUrl = Paths.serverUrl
      
      val transform = {
        "#error *" #> emailBody
      }

      sendEmail(subject, "help@mypetdefense.com", transform(template), "error@mypetdefense.com")
  }
}

trait SendTppApiJsonEmailHandling extends EmailHandlerChain {
  addHandler {
    case SendTppApiJsonEmail(emailBody) =>
      val template =
        Templates("emails-hidden" :: "tpp-api-json-email" :: Nil) openOr NodeSeq.Empty
      
      val subject = "[JSON] API Manual Backup"
      val hostUrl = Paths.serverUrl
      
      val transform = {
        "#json *" #> emailBody
      }

      sendEmail(subject, "mike.rivera@mypetdefense.com", transform(template))
  }
}

trait NotifyParentGrowthRateHandling extends EmailHandlerChain {
  addHandler {
    case NotifyParentGrowthRate(pet, newProduct, user) =>
      val template =
        Templates("emails-hidden" :: "notify-growth-rate" :: Nil) openOr NodeSeq.Empty
      
      val petName = pet.name.get
      val subject = s"$petName is growing and we're here to help!"
      val hostUrl = Paths.serverUrl
      
      val email = user.email.get
      
      val transform = {
        ".puppy-name *" #> petName &
        ".first-name *" #> user.firstName.get &
        ".new-product-size *" #> newProduct
      }

      sendEmail(subject, email, transform(template))
  }
}

trait SendShipmentRefundedEmailHandling extends EmailHandlerChain {
  addHandler {
    case SendShipmentRefundedEmail(parent, shipment) =>
      val template =
        Templates("emails-hidden" :: "shipment-refunded-email" :: Nil) openOr NodeSeq.Empty

      val dateFormat = new SimpleDateFormat("MMM dd, yyyy")
      
      val subject = s"Your Account has been Credited"
      val hostUrl = Paths.serverUrl
      
      val (email, firstName) = (parent.map { possibleParent =>
        if (possibleParent.status.get == Status.Cancelled) {
          val cancelledUser = CancelledUser.find(By(CancelledUser.user, possibleParent.userId.get))

          (cancelledUser.map(_.email.get).openOr(""), cancelledUser.map(_.firstName.get).openOr(""))
        } else {
          (possibleParent.email.get, possibleParent.firstName.get)
        }
      }).openOr(("",""))
      
      val transform = {
        ".first-name *" #> firstName &
        ".shipment-date *" #> tryo(dateFormat.format(shipment.dateProcessed.get)).openOr("") &
        ".shipment-amount *" #> shipment.amountPaid.get
      }

      sendEmail(subject, email, transform(template))
  }
}

trait DailySalesEmailHandling extends EmailHandlerChain {
  addHandler {
    case DailySalesEmail(
      agentNameAndCount,
      monthAgentNameAndCount,
      dailySalesByAgency,
      monthlySalesByAgency,
      email
    ) =>
      val template =
        Templates("emails-hidden" :: "daily-agent-report-email" :: Nil) openOr NodeSeq.Empty

      val yesterdayDate = LocalDateTime.now().minusDays(1)

      val subjectDate = yesterdayDate.format(DateTimeFormatter.ofPattern("MMM d", Locale.ENGLISH))
      val headerDate = yesterdayDate.format(DateTimeFormatter.ofPattern("MMMM d, yyyy", Locale.ENGLISH))
      val monthYear = yesterdayDate.format(DateTimeFormatter.ofPattern("MMMM yyyy", Locale.ENGLISH))
      
      val subject = s"[$subjectDate] Daily My Pet Defense Sales Report"
      val hostUrl = Paths.serverUrl

      val totalSales = agentNameAndCount.map(_._2).sum
      val monthlySales = monthAgentNameAndCount.map(_._2).sum
      
      val transform = {
        "#shield-logo [src]" #> (hostUrl + "/images/logo/shield-logo@2x.png") &
        ".new-sales *" #> totalSales &
        ".month-sales *" #> monthlySales &
        ".date *" #> headerDate &
        ".month *" #> monthYear &
        ".agent-container .agent" #> agentNameAndCount.map { case (agent, count) =>
          ".agent-name *" #> agent &
          ".sale-count *" #> count
        } &
        ".monthly-agent" #> monthAgentNameAndCount.map { case (agent, count) =>
          ".agent-name *" #> agent &
          ".sale-count *" #> count
        } &
        ".daily-agency" #> dailySalesByAgency.map { case (agencyName, count) =>
          ".agency-name *" #> agencyName &
          ".sale-count *" #> count
        } &
        ".monthly-agency-container .monthly-agency" #> monthlySalesByAgency.map { case (agencyName, count) =>
          ".agency-name *" #> agencyName &
          ".sale-count *" #> count
        }
      }

      sendEmail(subject, email, transform(template))
  }
}

trait InternalDailyEmailHandling extends EmailHandlerChain {
  addHandler {
    case InternalDailyEmail(
      newShipmentCount,
      paidShipmentCount,
      grossSales,
      cancelsCount,
      email
    ) =>
      val template =
        Templates("emails-hidden" :: "internal-daily-email" :: Nil) openOr NodeSeq.Empty
      
      val yesterdayDate = LocalDateTime.now().minusDays(1)

      val subjectDate = yesterdayDate.format(DateTimeFormatter.ofPattern("MMM d", Locale.ENGLISH))
      val headerDate = yesterdayDate.format(DateTimeFormatter.ofPattern("MMMM d, yyyy", Locale.ENGLISH))
      val monthYear = yesterdayDate.format(DateTimeFormatter.ofPattern("MMMM yyyy", Locale.ENGLISH))
      
      val subject = s"[$subjectDate] MPD Sales Report"
      val hostUrl = Paths.serverUrl

      val transform = {
        "#shield-logo [src]" #> (hostUrl + "/images/logo/shield-logo@2x.png") &
        ".date *" #> headerDate &
        ".total-shipments *" #> (newShipmentCount + paidShipmentCount) &
        ".gross-sales *" #> f"$$$grossSales%3.2f" &
        ".new-shipments *" #> newShipmentCount &
        ".paid-shipments *" #> paidShipmentCount &
        ".total-cancellations *" #> cancelsCount
      }

      sendEmail(subject, email, transform(template))
  }
}

trait TreatReceiptEmailHandling extends EmailHandlerChain {
  addHandler {
    case TreatReceiptEmail(
      order
    ) =>
      val template =
    Templates("emails-hidden" :: "treat-receipt-email" :: Nil) openOr NodeSeq.Empty

      val subject = "My Pet Defense Receipt"
      val email = order.email.get

      val treats = order.refresh.map(_.treatsOrdered.toList).openOr(Nil)
      val subtotal = (order.amountPaid.get - order.taxPaid.get)

      val transform = {
        "#parent-name *" #> order.firstName &
        ".name *" #> (order.firstName + " " + order.lastName) &
        "#ship-address-1 *" #> order.street1.get &
        "#ship-address-2" #> ClearNodesIf(order.street2.get == "") andThen
        "#ship-address-2-content *" #> order.street2.get &
        "#ship-city *" #> order.city.get &
        "#ship-state *" #> order.state.get &
        "#ship-zip *" #> order.zip.get &
        ".ordered-product" #> treats.map { orderedTreat =>
          val treat = orderedTreat.treat.obj

          ".treat-quantity *" #> orderedTreat.quantity.get &
          ".treat-name *" #> treat.map(_.name.get)
        } &
        ".amount-due *" #> f"$$$subtotal%2.2f" &
        "#tax-due *" #> f"$$${order.taxPaid.get}%2.2f" &
        "#total *" #> f"$$${order.amountPaid.get}%2.2f" 
      }

      sendEmail(subject, email, transform(template))
  }
}

trait TreatShippedEmailHandling extends EmailHandlerChain {
  addHandler {
    case TreatShippedEmail(
      order
    ) =>
      val template =
    Templates("emails-hidden" :: "treat-shipped-email" :: Nil) openOr NodeSeq.Empty

      val subject = "My Pet Defense Treats Shipped!"
      val email = order.email.get
      val trackingNumber = order.trackingNumber.get

      val trackingLink = s"https://tools.usps.com/go/TrackConfirmAction?tLabels=${trackingNumber}"

      val treats = order.treatsOrdered.toList
      val subtotal = (order.amountPaid.get - order.taxPaid.get)

      val transform = {
        "#ship-date" #> dateFormatter.format(new Date()) &
        ".tracking-link [href]" #> trackingLink &
        ".tracking-number *" #> trackingNumber &
        "#parent-name" #> order.firstName &
        ".name" #> (order.firstName + " " + order.lastName) &
        "#ship-address-1" #> order.street1.get &
        "#ship-address-2" #> ClearNodesIf(order.street2.get == "") andThen
        "#ship-address-2-content" #> order.street2.get &
        "#ship-city" #> order.city.get &
        "#ship-state" #> order.state.get &
        "#ship-zip" #> order.zip.get &
        ".ordered-product" #> treats.map { orderedTreat =>
          val treat = orderedTreat.treat.obj

          ".treat-quantity *" #> orderedTreat.quantity.get &
          ".treat-name *" #> treat.map(_.name.get)
        } &
        ".amount-due *" #> f"$$$subtotal%2.2f" &
        "#tax-due *" #> f"$$${order.taxPaid.get}%2.2f" &
        "#total *" #> f"$$${order.amountPaid.get}%2.2f" 
      }

      sendEmail(subject, email, transform(template))
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
      amountPaid,
      possibleTrackingNumber
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

      val trackingLink = s"https://tools.usps.com/go/TrackConfirmAction?tLabels=$possibleTrackingNumber"

      val dateFormatter = new SimpleDateFormat("MMM dd")

      val products = subscription.map(_.getProducts).openOr(Nil)
      val priceCode = subscription.map(_.priceCode.get).openOr("")

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
        "#total *" #> s"$$${amountPaid}" &
        ".with-tracking-number" #> ClearNodesIf(possibleTrackingNumber.isEmpty) andThen
        ".no-tracking-number" #> ClearNodesIf(!possibleTrackingNumber.isEmpty) andThen
        ".tracking-link [href]" #> trackingLink &
        ".tracking-number *" #> possibleTrackingNumber
      }
      
      sendEmail(subject, user.email.get, transform(invoicePaymentSucceededEmailTemplate))
  }
}

trait SixMonthSaleReceiptEmailHandling extends EmailHandlerChain {
  addHandler {
    case Send6MonthSaleReceipt(
      user,
      pets,
      subtotal,
      tax
    ) =>
      val template =
    Templates("emails-hidden" :: "six-month-receipt-email" :: Nil) openOr NodeSeq.Empty

      val subject = "My Pet Defense Receipt"
      val shipAddress = Address.find(By(Address.user, user), By(Address.addressType, AddressType.Shipping))

      val dateFormatter = new SimpleDateFormat("MMM dd")

      val products = pets.map(_.fleaTick.obj).flatten
      val amountPaid = subtotal + tax

      val transform = {
        ".receipt-date" #> dateFormatter.format(new Date()) &
        "#parent-name" #> user.firstName &
        ".name" #> user.name &
        "#ship-address-1" #> shipAddress.map(_.street1.get) &
        "#ship-address-2" #> ClearNodesIf(shipAddress.map(_.street2.get).getOrElse("") == "") andThen
        "#ship-address-2-content" #> shipAddress.map(_.street2.get) &
        "#ship-city" #> shipAddress.map(_.city.get) &
        "#ship-state" #> shipAddress.map(_.state.get) &
        "#ship-zip" #> shipAddress.map(_.zip.get) &
        "#tax" #> ClearNodesIf(tax == 0D) andThen
        ".ordered-product" #> products.map { product =>
          ".product *" #> product.name.get
        } &
        "#tax #tax-due *" #> f"$$$tax%2.2f" &
        "#total *" #> f"$$$amountPaid%2.2f" 
      }
      
      sendEmail(subject, user.email.get, transform(template))
  }
}

object EmailActor extends EmailActor
trait EmailActor extends EmailHandlerChain
                    with WelcomeEmailHandling
                    with FeedbackEmailHandling
                    with PictureEmailHandling
                    with NewPetAddedEmailHandling
                    with PetRemovedEmailHandling
                    with BillingUpdatedHandling
                    with AccountCancelledHandling
                    with ParentCancelledAccountHandling
                    with ParentPauseSubscriptionHandling
                    with ParentResumeSubscriptionHandling
                    with SendNewAdminEmailHandling
                    with SendNewAgentEmailHandling
                    with SendNewUserEmailHandling
                    with InvoicePaymentFailedEmailHandling
                    with InvoicePaymentSucceededEmailHandling 
                    with NewSaleEmailHandling 
                    with ResetPasswordHandling 
                    with CompleteResetPasswordHandling 
                    with ShipmentReadyEmailHandling 
                    with ContactUsEmailHandling
                    with Send5kEmailHandling
                    with SendAPIErrorEmailHandling
                    with SendTppApiJsonEmailHandling
                    with NotifyParentGrowthRateHandling
                    with DailySalesEmailHandling
                    with InternalDailyEmailHandling
                    with TreatReceiptEmailHandling
                    with TreatShippedEmailHandling
                    with SixMonthSaleReceiptEmailHandling
                    with SendShipmentRefundedEmailHandling
                    with TestimonialEmailHandling {

  val baseEmailTemplate = 
    Templates("emails-hidden" :: "email-template" :: Nil) openOr NodeSeq.Empty

  val valentineEmailTemplate = 
    Templates("emails-hidden" :: "valentine-email-template" :: Nil) openOr NodeSeq.Empty

  val reportingEmailTemplate = 
    Templates("emails-hidden" :: "reporting-email-template" :: Nil) openOr NodeSeq.Empty

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

  def sendEmail(subject: String, to: String, message: NodeSeq, fromEmail: String) {
    val emailTemplate = subject match {
      case valentine if subject.contains("Valentine") => 
        valentineEmailTemplate
      case dailyReport if subject.contains("Report") => 
        reportingEmailTemplate
      case _ => 
        baseEmailTemplate
    }

    val emailTransform = {
      "#content *" #> message &
      "#logo [src]" #> (hostUrl + "/images/logo/logo-name-white@2x.png") &
      "#user-email" #> to
    }
    
    val body = emailTransform(emailTemplate)

    val envSubj = envTag + subject

    Mailer.sendMail(
      From(fromEmail),
      Subject(envSubj),
      To(to),
      XHTMLMailBodyType(body)
    ) 
  }
}
