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

import java.util.{Date, Locale}
import java.time.{LocalDate, ZoneId, LocalDateTime}
import java.time.format.DateTimeFormatter

case class UpdateChartData(chartName: String, newData: Array[Int], newLabels: Array[String] = Array()) extends MyPetDefenseEvent("update-chart-data")

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
  val chartDateFormat = new SimpleDateFormat("MMM")

  val users = User.findAll(
    By(User.userType, UserType.Parent)
  ).filter(_.referer.obj == agency)
  
  val allSubscriptions = users.map(_.getSubscription).flatten
  val allShipments = allSubscriptions.map(_.shipments.toList).flatten

  val usersByStatus = users.groupBy(_.status.get)
  val activeUsers = usersByStatus(Status.Active)
  val activeUserCount = activeUsers.size
  val inactiveUserCount = users.size - activeUserCount

  val activeUserSubscription = users.map(_.getSubscription).flatten
  val activeShipments = activeUserSubscription.map(_.shipments.toList).flatten

  var currentParent: Box[User] = Empty

  def findStatus(status: Status.Value) = {
    if (status == Status.Active)
      "Active"
    else 
      "Inactive"
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

  def currentDate = LocalDateTime.now()
  
  def getDateRange(month: String) = {
    if (month == "") {
      currentDate
    } else {
      convertMonthToDate(month)
    }
  }

  def getProcessDateOfShipment(shipment: Shipment) = {
    shipment.dateProcessed.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()
  }

  def findCurrentMonthShipment(shipment: Shipment, month: String = "") = {
    val date = getDateRange(month)

    val processedDate = getProcessDateOfShipment(shipment)

    (
      (processedDate.getYear == date.getYear) &&
      (processedDate.getMonth == date.getMonth)
    )
  }

  def convertMonthToDate(month: String) = {
    val dateFormat = new SimpleDateFormat("MMMM yyyy")
    val monthDate = dateFormat.parse(s"$month 2018") //TODO: dynanmic year

    monthDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime()
  }

  def updateCharts = {
    val activeUserSignupDates = activeUserSubscription.map(_.startDate.get)

    val activeUsersSignupByMonth = activeUserSignupDates.map(chartDateFormat.format).groupBy(identity).mapValues(_.size)

    val activeUsersSignupByMonthLabel = activeUsersSignupByMonth.keys.toArray
    val activeUsersSignupByMonthValue = activeUsersSignupByMonth.values.toArray

    val shipmentsByMonth = allShipments.map(_.dateProcessed.get).map(chartDateFormat.format).groupBy(identity).mapValues(_.size)

    val allShipmentsByMonthLabel = shipmentsByMonth.keys.toArray
    val allShipmentsByMonthValue = shipmentsByMonth.values.toArray

    (
      UpdateChartData("activeInactive", Array(activeUserCount, inactiveUserCount)) &
      UpdateChartData("totalActive", allShipmentsByMonthValue, allShipmentsByMonthLabel) &
      UpdateChartData("signup", activeUsersSignupByMonthValue, activeUsersSignupByMonthLabel)
    )
  }

  def snapShotBindings = {
    val currentMonthSubscriptionShipments = {
      for {
        subscription <- allSubscriptions
        shipment <- subscription.shipments.toList
          if (findCurrentMonthShipment(shipment)) 
      } yield {
        (subscription, shipment)
      } 
    }

    ".mtd-shipments *" #> currentMonthSubscriptionShipments.size
    //".mtd-commission-earned *" #>
    //".ytd-commission-earned *" #>

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
    snapShotBindings &
    ".overview [class+]" #> "current" &
    ".store-name *" #> currentUser.map(_.name) &
    ".update-data [onclick]" #> ajaxInvoke(() => updateCharts) &
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
