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

import scala.collection.immutable.ListMap

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
  val chartDateFormat = new SimpleDateFormat("MM")
  val chartDateFormatName = new SimpleDateFormat("MMM")

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

  val usersWithName = users.map(updateUserName(_))

  var currentParent: Box[User] = Empty

  def findStatus(status: Status.Value) = {
    if (status == Status.Active)
      "Active"
    else 
      "Inactive"
  }

  def updateUserName(parent: User) = {
    if (parent.status == Status.Cancelled) {
      val cancelledUser = findCancelledUserName(parent)
      parent
        .firstName(cancelledUser.map(_.firstName.get).openOr(""))
        .lastName(cancelledUser.map(_.lastName.get).openOr(""))
    } else {
      parent
    }
  }

  def findCancelledUserName(parent: User) = {
    CancelledUser.find(By(CancelledUser.user, parent.userId.get))
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

  def findCurrentYearSubscriptionSignup(subscription: Subscription) = {
    val signupDate = subscription.startDate.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()

    signupDate.getYear == currentDate.getYear
  }

  def findCurrentMonthSubscriptionSignup(subscription: Subscription) = {
    val signupDate = subscription.startDate.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()

    signupDate.getMonth == currentDate.getMonth
  }

  def convertMonthToDate(month: String) = {
    val dateFormat = new SimpleDateFormat("MMMM yyyy")
    val monthDate = dateFormat.parse(s"$month 2018") //TODO: dynanmic year

    monthDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime()
  }

  def updateCharts = {
    val activeUserSignupDates = activeUserSubscription.map(_.startDate.get)

    val activeUsersSignupByMonth = activeUserSignupDates.map(chartDateFormat.format).map(toInt).groupBy(identity).mapValues(_.size)

    val activeUsersSignupByMonthSorted = ListMap(activeUsersSignupByMonth.toSeq.sortBy(_._1):_*)

    val activeUsersSignupByMonthLabel = activeUsersSignupByMonthSorted.keys.toArray.map { month =>
      chartDateFormatName.format(chartDateFormat.parse(month.toString))
    }
    val activeUsersSignupByMonthValue = activeUsersSignupByMonthSorted.values.toArray

    val shipmentsByMonth = allShipments.map(_.dateProcessed.get).map(chartDateFormat.format).map(toInt).groupBy(identity).mapValues(_.size)

    val adjustedShipmentsByMonth = shipmentsByMonth + (12 -> activeUserCount)

    val shipmentsByMonthSorted = ListMap(adjustedShipmentsByMonth.toSeq.sortBy(_._1):_*)

    val allShipmentsByMonthLabel = shipmentsByMonthSorted.keys.toArray.map { month =>
      chartDateFormatName.format(chartDateFormat.parse(month.toString))
    }

    val allShipmentsByMonthValue = shipmentsByMonthSorted.values.toArray

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

    val currentYearSignups = {
      for {
        subscription <- allSubscriptions
          if (findCurrentYearSubscriptionSignup(subscription)) 
      } yield {
        subscription
      }
    }

    val currentMonthSignups = {
      for {
        subscription <- currentYearSignups
          if (findCurrentMonthSubscriptionSignup(subscription)) 
      } yield {
        subscription
      }
    }

    val ytdCommission = currentYearSignups.size * 12
    val mtdCommission = currentMonthSignups.size * 12

    ".mtd-shipments *" #> currentMonthSubscriptionShipments.size &
    ".mtd-commission-earned *" #> s"$$$mtdCommission" &
    ".ytd-commission-earned *" #> s"$$$ytdCommission"
  }

  def petBindings = {
    val parent = currentParent
    val pets = parent.map(_.pets.toList).openOr(Nil)

    ".pet" #> pets.map { pet =>
      ".pet-name *" #> pet.name.get &
      ".pet-status *" #> findStatus(pet.status.get) &
      ".pet-status [class+]" #> findStatus(pet.status.get).toLowerCase &
      ".current-product *" #> pet.product.map(_.getSizeAndSizeName)
    }
  }

  def render = {
    snapShotBindings &
    ".overview [class+]" #> "current" &
    ".store-name *" #> currentUser.map(_.name) &
    ".update-data [onclick]" #> ajaxInvoke(() => updateCharts) &
    ".customer-count *" #> usersWithName.size &
    "tbody" #> usersWithName.sortWith(_.name < _.name).map { parent =>
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

          ".name *" #> parent.name &
          ".status *" #> findStatus(parent.status.get) &
          ".status [class+]" #> findStatus(parent.status.get).toLowerCase &
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
