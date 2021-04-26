package com.mypetdefense.snippet
package agency

import com.mypetdefense.model._
import com.mypetdefense.service.{StripeBoxAdapter => Stripe}
import com.mypetdefense.util.Paths.{adminUser, loggedIn}
import com.mypetdefense.util.SecurityContext
import com.stripe.param.CustomerCreateParams
import net.liftweb.common._
import net.liftweb.http.SHtml.{hidden, text}
import net.liftweb.http._
import net.liftweb.http.js.JsCmds.Alert
import net.liftweb.mapper.By
import net.liftweb.sitemap.Menu
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

object CustomerUpload extends Loggable {
  val menu: Menu.Menuable = Menu.i("Customer Upload") / "agency" / "customer-upload" >>
    adminUser >>
    loggedIn
}

class CustomerUpload extends Loggable {
  val currentUser: Box[User] = SecurityContext.currentUser
  val agency: Box[Agency] = currentUser.flatMap(_.agency.obj)

  var customerEmail = ""
  var cardToken = ""

  def createNewCustomer(): Alert = {
    val possibleParent = NewParent(
      firstName = "",
      lastName = "",
      email = customerEmail,
      address = NewAddress("", None, "", "", ""),
      phone = None,
      stripeToken = cardToken

    )
    val existingUser =
      User.find(By(User.email, possibleParent.email), By(User.userType, UserType.Parent))

    existingUser match {
      case Empty =>
        val currentParent = User.createNewPendingUser(
          possibleParent,
          agency,
          ""
        )

        Address.createNewAddress(possibleParent.address, Full(currentParent))

        val stripeCustomer =
          Stripe.Customer.create(
            CustomerCreateParams.builder
              .setEmail(currentParent.email.get)
              .setSource(possibleParent.stripeToken)
              .build
          )

        stripeCustomer match {
          case Full(_) => Alert(s"Created $customerEmail")
          case Failure(errorMsg, _, _) => Alert(s"Failed to create $customerEmail with error $errorMsg.")
          case _ => Alert(s"Failed to create $customerEmail. Unknown error.")
        }

      case Full(user) =>
        Alert(s"User found for email ${user.email.get}. No user created.")

      case _ =>
        Alert(s"Error when attempting to use email $customerEmail. No user created.")
    }
  }

  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
    "#customer-email" #> text("", customerEmail = _) &
    "#stripe-token" #> hidden(cardToken = _, cardToken) &
    "#create-customer" #> SHtml.ajaxSubmit("Add Customer", () => createNewCustomer())
  }
}