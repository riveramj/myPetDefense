package com.mypetdefense.snippet

import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.model.domain.action.SystemAction.{SystemCanceledAccount, SystemRemovedPet}
import com.mypetdefense.service._
import com.mypetdefense.util.SecurityContext
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.json._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._
import org.joda.time._

import scala.language.postfixOps

object StripeHook extends StripeHook {
  override val emailActor: EmailActor = EmailActor
}

trait StripeHook extends RestHelper with Loggable {
  def emailActor: EmailActor

  def cancelledAccountWithPayment(objectJson: JValue): Box[OkResponse] = ({
    for {
      stripeCustomerId <- tryo((objectJson \ "customer").extract[String]) ?~! "No customer."
      parent           <- User.find(By(User.stripeId, stripeCustomerId)) ?~! "No user found"
      subscription     = parent.subscription.obj
      if subscription.isEmpty
    } yield {
      val pets: List[Pet] = parent.pets.toList

      pets.map { pet =>
        val actionLog = SystemRemovedPet(
          SecurityContext.currentUserId,
          None,
          pet.petId.get,
          pet.name.get
        )

        ParentService.removePet(parent, pet, actionLog)
      }

      val actionLog = SystemCanceledAccount(
        parent.userId.get,
        None,
        parent.subscription.obj.map(_.subscriptionId.get).openOr(0L)
      )

      ParentService.removeParent(parent, actionLog)
      Full(OkResponse())
    }
  }).openOr(Failure("couldnt cancel user in MPD and Stripe"))

  def invoicePaymentSucceeded(objectJson: JValue): Box[OkResponse] = {
    for {
      stripeCustomerId     <- tryo((objectJson \ "customer").extract[String]) ?~! "No customer."
      stripeSubscriptionId <- tryo((objectJson \ "subscription").extract[String]) ?~! "No subscription id."
      subtotal             <- tryo((objectJson \ "subtotal").extract[String]) ?~! "No subtotal"
      tax                  <- tryo((objectJson \ "tax").extract[String]) ?~! "No tax paid"
      amountPaid           <- tryo((objectJson \ "amount_due").extract[String]) ?~! "No amount paid"
      user                 <- User.find(By(User.stripeId, stripeCustomerId)) ?~! "No user found"
      subscription         <- user.subscription ?~! "No mpd subscription found"
      shippingAddress <- Address.find(
                          By(Address.user, user),
                          By(Address.addressType, AddressType.Shipping)
                        ) ?~! "No mpd address found"
      invoicePaymentId <- tryo((objectJson \ "id").extract[String]) ?~! "No ID."
    } yield {
      val coupon = user.coupon.obj
      val charge = tryo((objectJson \ "charge").extract[String])

      val notTrial_?   = ParentService.notTrialSubscription_?(stripeSubscriptionId)
      val city         = shippingAddress.city.get
      val state        = shippingAddress.state.get
      val zip          = shippingAddress.zip.get
      val activePets_? = user.activePets.nonEmpty

      def formatAmount(possibleAmount: String) = {
        val formattedAmount = tryo(possibleAmount.toDouble / 100.0).openOr(0d)

        if (formattedAmount == 0d)
          0.toString
        else
          f"$formattedAmount%2.2f"
      }

      if (notTrial_? && activePets_?) {
        subscription.reload.status(Status.Active).save()

        TaxJarService.processTaxesCharged(
          invoicePaymentId,
          city,
          state,
          zip,
          formatAmount(subtotal),
          formatAmount(tax)
        )

        //ParentService.updatePuppyProducts(user)

        ShipmentService.createNewShipment(
          user,
          invoicePaymentId,
          charge,
          formatAmount(amountPaid),
          formatAmount(tax)
        )
      }

      if (coupon.map(_.couponCode.get == "20off").openOr(false))
        ParentService.updateStripeSubscriptionTotal(user)

      OkResponse()
    }
  }

  def invoicePaymentFailed(objectJson: JValue): Box[OkResponse] = {
    for {
      stripeCustomerId   <- tryo((objectJson \ "customer").extract[String]) ?~! "No customer."
      user               <- User.find(By(User.stripeId, stripeCustomerId))
      subscription       <- user.subscription
      totalAmountInCents <- tryo((objectJson \ "total").extract[Long]) ?~! "No total."
    } yield {
      val nextPaymentAttemptSecs: Option[Long] =
        tryo((objectJson \ "next_payment_attempt").extract[Option[Long]]).openOr(None)

      val nextPaymentAttempt = nextPaymentAttemptSecs.map { nextPaymentAttemptInSecs =>
        new DateTime(nextPaymentAttemptInSecs * 1000)
      } filter {
        _ isAfterNow
      }
      val amount = totalAmountInCents / 100d
      emailActor ! SendInvoicePaymentFailedEmail(user, amount, nextPaymentAttempt)

      OkResponse()
    }
  }

  def subscriptionPastDue(objectJson: JValue): Box[OkResponse] = {
    for {
      stripeCustomerId <- tryo((objectJson \ "customer").extract[String]) ?~! "No customer."
      user             <- User.find(By(User.stripeId, stripeCustomerId))
      accountStatus    <- tryo((objectJson \ "status").extract[String]) ?~! "No status."
    } yield {
      if (accountStatus == "past_due")
        user.subscription.map(_.status(Status.BillingSuspended).saveMe)

      OkResponse()
    }
  }

  serve {
    case req @ Req("stripe-hook" :: Nil, _, PostRequest) =>
      for {
        requestBody <- req.body
        requestJson <- tryo(Serialization.read[JValue](new String(requestBody)))
        id          <- (requestJson \ "id").extractOpt[String]
        eventType   <- (requestJson \ "type").extractOpt[String]
        dataJson   = (requestJson \ "data")
        objectJson = (dataJson \ "object")
      } yield {
        val result: Box[LiftResponse] = eventType match {
          case "invoice.payment_succeeded"     =>
            val reconciliationAttempt = invoicePaymentSucceeded(objectJson)
            if (reconciliationAttempt.isDefined)
              reconciliationAttempt
            else
              cancelledAccountWithPayment(objectJson)
          case "invoice.payment_failed"        => invoicePaymentFailed(objectJson)
          case "customer.subscription.updated" => subscriptionPastDue(objectJson)
          case _                               => Full(OkResponse())
        }

        result match {
          case Full(resp) if resp.isInstanceOf[OkResponse] => resp
          case Full(resp)                                  => resp
          case Empty                                       => NotFoundResponse()
          case Failure(msg, _, _)                          => PlainTextResponse(msg, Nil, 500)
        }
      }
  }
}
