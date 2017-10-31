package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.Paths._
import com.mypetdefense.actor._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.util.SecurityContext._
import com.mypetdefense.service.{ParentService, TaxJarService}

import me.frmr.stripe.Discount

object AccountOverview extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Account Overview") / "account-overview" >>
    loggedIn >>
    parent
}

class AccountOverview extends Loggable {
  val user = currentUser
  val subscription = user.flatMap(_.getSubscription)
  val priceCode = subscription.map(_.priceCode.get).getOrElse("")

  val shippingAddress = Address.find(
    By(Address.user, user),
    By(Address.addressType, AddressType.Shipping)
  )

  val pets = user.map { parent => 
    Pet.findAll(By(Pet.user, parent))
  }.openOr(Nil)

  val dateFormat = new SimpleDateFormat("MMMM dd, yyyy")

  val stripeCustomerId = user.map(_.stripeId.get).openOr("")

  val upcomingInvoice = ParentService.getUpcomingInvoice(stripeCustomerId)

  val productSubtotal = upcomingInvoice.map(_.subtotal/100D).openOr(0D)

  val discountPercent: Box[Double] = {
    for {
      invoice <- upcomingInvoice
      discount <- invoice.discount
      coupon = discount.coupon
      discount <- coupon.percentOff
    } yield {
      discount.toDouble/100D
    }
  }

  val discountAmountRaw = discountPercent.map(_ * productSubtotal).openOr(0D)
  
  val discountAmount = discountAmountRaw match {
    case 0 => "-$0.00"
    case _ => f"-$$$discountAmountRaw%2.2f"
  }

  val taxDue = upcomingInvoice.flatMap(_.tax.map(_.toDouble/100D)).openOr(0D)
  val totalDue = upcomingInvoice.map(_.amountDue.toDouble/100D).openOr(0D)

  def render = {
    "#page-body-container" #> {
      user.map { parent =>
        val subscription = parent.getSubscription
        val nextShipDate = subscription.map(_.nextShipDate.get)

        val petBindings = {
          ".pet" #> pets.map { pet =>
            val product = pet.product.obj
            val priceItem = product.flatMap { item =>
              Price.getPricesByCode(item, priceCode)
            }
            val price = priceItem.map(_.price.get).getOrElse(0D)

            ".pet-name *" #> pet.name &
            ".pet-product *" #> product.map(_.name.get) & 
            ".pet-size *" #> product.map(_.getSizeAndSizeName) &
            ".price *" #> f"$$$price%2.2f"
          }
        }

        "#upcoming-order a [class+]" #> "current" &
        "#user-email *" #> parent.email &
        ".next-ship-date *" #> nextShipDate.map(dateFormat.format(_)) &
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
