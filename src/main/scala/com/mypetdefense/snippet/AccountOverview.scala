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

  def render = {
    val dateFormat = new SimpleDateFormat("MMMM dd, yyyy")
    
    "#page-body-container" #> {
      user.map { parent =>
        val subscription = parent.getSubscription
        val nextShipDate = subscription.map(_.nextShipDate.get)

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
        }
      }
    }
  }
}
