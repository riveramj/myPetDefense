package com.mypetdefense.snippet.customer

import java.text.SimpleDateFormat
import java.util.Date

import com.mypetdefense.model._
import com.mypetdefense.service.{ParentService, StripeBoxAdapter => Stripe}
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.util.SecurityContext._
import net.liftweb.common._
import net.liftweb.mapper.By
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._

object AccountOverview extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Account Overview") / "account-overview" >>
    loggedIn >>
    parent
}

class AccountOverview extends Loggable {
  val oldUser: Box[User]              = currentUser
  val user: Box[User]                 = currentUser.map(_.reload)
  val subscription: Box[Subscription] = user.flatMap(_.subscription).map(_.reload)
  val priceCode: String               = subscription.map(_.priceCode.get).getOrElse("")

  val shippingAddress: Box[Address] = Address.find(
    By(Address.user, user),
    By(Address.addressType, AddressType.Shipping)
  )

  val dateFormat = new SimpleDateFormat("MMMM dd, yyyy")

  val stripeCustomerId: String = user.map(_.stripeId.get).openOr("")

  val upcomingInvoice: Box[Stripe.Invoice] = ParentService.getUpcomingInvoice(stripeCustomerId)

  val productSubtotal: Double =
    upcomingInvoice.flatMap(_.subtotal).map(_ / 100d).openOr(0d)

  val discount: Box[Double] = {
    for {
      invoice  <- upcomingInvoice
      discount <- invoice.discount
      coupon   <- discount.coupon
    } yield {
      val percentOff = coupon.percentOff.map(_.toInt).getOrElse(0)
      val amountOff  = coupon.amountOff.getOrElse(0L)

      if (percentOff > 0)
        (percentOff.toDouble / 100d) * productSubtotal
      else if (amountOff > 0)
        amountOff.toDouble / 100d
      else
        0d
    }
  }

  val discountAmount: String = discount match {
    case Full(discount) if discount > 0 => f"-$$$discount%2.2f"
    case _                              => "-$0.00"
  }

  val taxDue: Double =
    upcomingInvoice.flatMap(_.tax).map(_.toDouble / 100d).openOr(0d)
  val totalDue: Double =
    upcomingInvoice.flatMap(_.amountDue).map(_.toDouble / 100d).openOr(0d)
  val nextBillDateRaw: Box[Long] =
    upcomingInvoice.flatMap(_.created)

  val nextBillDate: Box[Date] = nextBillDateRaw.map { date => new Date(date * 1000) }

  def render: CssSel = {
    "#page-body-container" #> {
      user.map { parent =>
        val nextShipDate = subscription.map(_.nextShipDate.get)
        val upgradedSubscription = subscription.map(_.isUpgraded.get).openOr(false)
        val boxes = subscription.map(_.subscriptionBoxes.toList.filter(_.status.get == Status.Active)).openOr(Nil)

        val petBindings = {
          ".pet" #> boxes.map { box =>
            val pet     = box.pet.obj
            val product = box.fleaTick.obj
            val price = SubscriptionBox.findBoxPrice(box)

            ".pet-name *" #> pet.map(_.name.get) &
            ".pet-product *" #> product.map(_.name.get) &
            ".pet-size *" #> product.map(_.getSizeAndSizeName) &
            ".price *" #> f"$$$price%2.2f"
          }
        }

        "#upcoming-order a [class+]" #> "current" &
        "#user-email *" #> parent.email &
        ".next-ship-date *" #> nextShipDate.map(dateFormat.format) &
        ".status *" #> subscription.map(_.status.get.toString) &
        ".next-bill-date *" #> nextBillDate.map(dateFormat.format) &
        "#user-address" #> shippingAddress.map { address =>
          "#name *" #> parent.name &
          "#address-one *" #> address.street1.get &
          "#address-two *" #> ClearNodesIf(address.street2.get == "") &
          "#address-two *" #> address.street2.get &
          "#city *" #> address.city.get &
          "#state *" #> address.state.get &
          "#zip *" #> address.zip.get
        } &
        petBindings &
        ".subtotal *" #> f"$$$productSubtotal%2.2f" &
        ".discount *" #> discountAmount &
        ".tax-charge" #> ClearNodesIf(taxDue == 0d) &
        ".tax-charge .tax *" #> f"$taxDue%2.2f" &
        ".total *" #> f"$$$totalDue%2.2f"
      }
    }
  }
}
