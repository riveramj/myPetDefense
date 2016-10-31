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
import com.mypetdefense.service.TaxJarService

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
  val shippingAddress = Address.find(
    By(Address.user, user),
    By(Address.addressType, AddressType.Shipping)
  )

  val pets = user.map { parent => 
    Pet.findAll(By(Pet.user, parent))
  }.openOr(Nil)

  val productSubtotal = pets.size * 9.99

  def render = {
    val dateFormat = new SimpleDateFormat("MMMM dd, yyyy")

    val taxDue = {
      if (shippingAddress.map(_.state.get.toLowerCase) == Full("ga")) {
        shippingAddress.map { address =>
          val taxInfo = TaxJarService.findTaxAmoutAndRate(
            address.city.get,
            address.state.get,
            address.zip.get,
            (pets.size * 9.99)
          )

          taxInfo._1
        }.openOr(0D)
      } else {
        0D
      }
    }

    val totalDue = productSubtotal + taxDue

    "#page-body-container" #> {
      user.map { parent =>
        val subscription = parent.getSubscription
        val nextShipDate = subscription.map(_.nextShipDate.get)

        val petBindings = {
          ".pet" #> pets.map { pet =>
            ".pet-name *" #> pet.name &
            ".pet-product *" #> pet.product.obj.map(_.name.get) & 
            ".pet-size *" #> pet.product.obj.map(_.size.get.toString)
          }
        }

        "#upcoming-order [class+]" #> "current" &
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
        ".multipet-discount" #> ClearNodesIf(pets.size == 1) &
        ".tax-charge" #> ClearNodesIf(taxDue == 0D) &
        ".tax *" #> f"$taxDue%2.2f" &
        ".total *" #> f"$$$totalDue%2.2f"
      }
    }
  }
}
