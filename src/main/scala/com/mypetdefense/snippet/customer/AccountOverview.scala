package com.mypetdefense.snippet.customer

import java.text.SimpleDateFormat
import java.util.Date

import com.mypetdefense.model._
import com.mypetdefense.service.ParentService
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.util.SecurityContext._
import net.liftweb.common._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._

object AccountOverview extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu = Menu.i("Account Overview") / "account-overview" >>
    loggedIn >>
    parent
}

class AccountOverview extends Loggable {
  val oldUser = currentUser
  val user = currentUser.flatMap(_.refresh)
  val pets = user.map(_.activePets).openOr(Nil)
  val subscription = user.flatMap(_.subscription).flatMap(_.refresh)
  val priceCode = subscription.map(_.priceCode.get).getOrElse("")

  val shippingAddress = Address.find(
    By(Address.user, user),
    By(Address.addressType, AddressType.Shipping)
  )

  val dateFormat = new SimpleDateFormat("MMMM dd, yyyy")

  val stripeCustomerId = user.map(_.stripeId.get).openOr("")

  val upcomingInvoice = ParentService.getUpcomingInvoice(stripeCustomerId)

  val productSubtotal = upcomingInvoice.map(_.subtotal/100D).openOr(0D)

  val discount: Box[Double] = {
    for {
      invoice <- upcomingInvoice
      discount <- invoice.discount
      coupon = discount.coupon
    } yield {
      val percentOff = coupon.percentOff.getOrElse(0)
      val amountOff = coupon.amountOff.getOrElse(0L)

      if (percentOff > 0)
        (percentOff.toDouble/100D) * productSubtotal
      else if (amountOff > 0)
        amountOff.toDouble/100D
      else
        0D
    }
  }

  val discountAmount = discount match {
    case Full(discount) if discount > 0 => f"-$$$discount%2.2f"
    case _ => "-$0.00"
  }

  val taxDue = upcomingInvoice.flatMap(_.tax.map(_.toDouble/100D)).openOr(0D)
  val totalDue = upcomingInvoice.map(_.amountDue.toDouble/100D).openOr(0D)
  val nextBillDateRaw = upcomingInvoice.map(_.date)

  val nextBillDate = nextBillDateRaw.map { date =>
    new Date(date * 1000)
  }

  def render = {
    "#page-body-container" #> {
      user.map { parent =>
        val nextShipDate = subscription.map(_.nextShipDate.get)
        val boxes = subscription.map(_.subscriptionBoxes.toList).openOr(Nil)

        val petBindings = {
          ".pet" #> boxes.map { box =>
            val pet = box.pet.obj
            val product = box.fleaTick.obj
            val price = if (SubscriptionBox.possiblePrice(box) == 0D) {
              product.flatMap { item =>
                Price.getPricesByCode(item, priceCode).map(_.price.get)
              }.openOr(0D)
            } else {
              SubscriptionBox.possiblePrice(box)
            }

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
        ".tax-charge" #> ClearNodesIf(taxDue == 0D) &
        ".tax-charge .tax *" #> f"$taxDue%2.2f" &
        ".total *" #> f"$$$totalDue%2.2f"
      }
    }
  }
}
