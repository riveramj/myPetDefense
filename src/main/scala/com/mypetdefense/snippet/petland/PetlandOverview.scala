package com.mypetdefense.snippet
package petland

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
  import js.JsCmds._
import net.liftweb.mapper.By

import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service.{CouponService, ReportingService}
import com.mypetdefense.util.{ClearNodesIf, SecurityContext}

import java.text.SimpleDateFormat

object PetlandOverview extends Loggable { 
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Petland Overview") / "petland" / "store-overview" >>
    agencyUser >>
    petlandUser >>
    loggedIn
}

class PetlandOverview extends Loggable {
  val currentUser = SecurityContext.currentUser
  val agency = currentUser.flatMap(_.agency.obj)
  val agencyName = agency.map(_.name.get).openOr("")
  val signupCancelDateFormat = new SimpleDateFormat("MM/dd/yyyy")
  
  var currentParent: Box[User] = Empty

  val users = User.findAll(
    By(User.userType, UserType.Parent)
  ).filter(_.referer.obj == agency)

  def findStatus(status: Status.Value) = {
    if (status == Status.Active)
      "Active"
    else if (status == Status.Cancelled)
      "Cancelled"
    else 
      "Suspended"
  }

  def getName(parent: User) = {
    if (parent.status == Status.Cancelled)
      findCancelledUserName(parent)
    else
      parent.name
  }

  def findCancelledUserName(parent: User) = {
    CancelledUser.find(By(CancelledUser.user, parent.userId.get)).map(_.name).openOr("")
  }

  def petBindings = {
    val parent = currentParent
    val pets = parent.map(_.pets.toList).openOr(Nil)

    ".pet" #> pets.map { pet =>
      ".pet-name *" #> pet.name.get &
      ".pet-status *" #> findStatus(pet.status.get) &
      ".current-product *" #> pet.product.map(_.getSizeAndSizeName)
    }
  }

  def render = {
    ".overview [class+]" #> "current" &
    ".store-name *" #> currentUser.map(_.name) &
    "tbody" #> users.sortWith(_.name < _.name).map { parent =>
      idMemoize { detailsRenderer =>

        ".user" #> {
          val subscription = parent.getSubscription

          val signupDate = subscription.map { sub =>
            signupCancelDateFormat.format(sub.startDate.get)
          }.getOrElse("")

          val cancellationDate = {
            if (parent.status.get == Status.Cancelled) {
              val possibleCancelDate = subscription.map(_.cancellationDate.get)
              possibleCancelDate.flatMap { date =>
                tryo(signupCancelDateFormat.format(date))
              }.getOrElse("")
            } else {
              "-"
            }
          }
          
          val shipmentCount = subscription.map(_.shipments.toList.size).getOrElse(0)

          ".name *" #> getName(parent) &
          ".status *" #> findStatus(parent.status.get) &
          ".signup-date *" #> signupDate &
          ".cancel-date *" #> cancellationDate &
          ".shipment-count *" #> shipmentCount
        } &
        "^ [onclick]" #> ajaxInvoke(() => {
          if (currentParent.isEmpty) {
            currentParent = Full(parent)
          } else {
            currentParent = Empty
          }

          detailsRenderer.setHtml
        }) &
        ".info [class+]" #> {if (currentParent.isEmpty) "" else "expanded"} &
        "^ [class+]" #> {if (currentParent.isEmpty) "" else "expanded"} &
        petBindings
      }
    }
  }
}
