package com.mypetdefense.snippet
package agency

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

object AgencyOverview extends Loggable { 
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  def agencyCusomterExport: Box[LiftResponse] = {
    exportAgencyCustomerMenu.currentValue flatMap { agency =>
      ReportingService.exportAgencyCustomers(agency)
    } 
  }
  
  val menu = Menu.i("Agency Overview") / "agency" / "agency-overview" >>
    adminUser >>
    loggedIn

  val exportAgencyCustomerMenu = Menu.param[String](
    "Agency Customers Export",
    "Agency Customers Export",
    Full(_),
    string => string
  ) / "agency" / "agency-overview" / "customers.csv" >>
  adminUser >>
  loggedIn >>
  EarlyResponse(agencyCusomterExport _)
}

class AgencyOverview extends Loggable {
  val currentUser = SecurityContext.currentUser
  val signupCancelDateFormat = new SimpleDateFormat("MM/dd/yyyy")
  val chartDateFormat = new SimpleDateFormat("MM")
  val chartDateFormatName = new SimpleDateFormat("MMM")

  val allUsers = User.findAll(
    By(User.userType, UserType.Parent)
  )

  val agencies = {
    if (currentUser.map(_.petlandData_?).openOr(false)) {
      Agency.findAll(By(Agency.petlandStore, true))
    } else {
      currentUser.flatMap(_.agency.obj).toList
    }
  }

  var currentParent: Box[User] = Empty
  var dateFilterTransform: Box[IdMemoizeTransform] = Empty
  var agencyRenderer: Box[IdMemoizeTransform] = Empty

  def updateAgencyName = {
    chosenAgency.map(_.name.get).getOrElse("All Stores")
  }

  def updateAgencyUsers = {
    if (chosenAgency.isEmpty) {
      agencies.map { agency =>
        allUsers.filter(_.referer.obj == Full(agency))
      }.flatten
    } else {
      allUsers.filter(_.referer.obj == chosenAgency)
    }
  }

  var chosenAgency: Box[Agency] = {
    if (agencies.size > 1)
      Empty
    else
      agencies.headOption
  }

  var agencyName = updateAgencyName
  var users = updateAgencyUsers
  
  def allSubscriptions = users.flatMap(_.subscription.obj)
  def allShipments = allSubscriptions.flatMap(_.shipments.toList)

  def usersByStatus = users.groupBy(_.status.get)
  def activeUsers = tryo(usersByStatus(Status.Active)).openOr(Nil)
  def activeUserCount = activeUsers.size
  def inactiveUserCount = users.size - activeUserCount

  def activeUserSubscription = users.flatMap(_.subscription.obj)
  def activeShipments = activeUserSubscription.flatMap(_.shipments.toList)

  def usersWithName = users.map(updateUserName)

  def shipmentsByMonth = allShipments.map(_.dateProcessed.get).map(chartDateFormat.format).map(toInt).groupBy(identity).mapValues(_.size)

  def adjustedShipmentsByMonth = shipmentsByMonth + (12 -> activeUserCount)

  def shipmentsByMonthSorted = ListMap(adjustedShipmentsByMonth.toSeq.sortBy(_._1):_*)

  def allShipmentsByMonthLabel = shipmentsByMonthSorted.keys.toArray.map { month =>
    chartDateFormatName.format(chartDateFormat.parse(month.toString))
  }

  var monthDateFilter = "All Months"
  var storeIdFilter = chosenAgency.map(_.agencyId.get).openOr(-1L).toString

  def storeDropdown = {
    val agencyList = {
      if (agencies.size == 1) {
        agencies.map(agency => (agency.agencyId.get.toString, agency.name.get))
      } else {
        List((-1L.toString, "All Stores")) ++ agencies.map(agency => (agency.agencyId.get.toString, agency.name.get))
      }
    }

    SHtml.ajaxSelect(
      agencyList,
      Full(storeIdFilter),
      (possibleAgencyId: String) => {
        storeIdFilter = possibleAgencyId
        chosenAgency = {
          if (storeIdFilter == -1L.toString)
            Empty
          else {
            val properAgencyId = tryo(storeIdFilter.toLong).openOr(-1L)
            agencies.find(_.agencyId.get == properAgencyId)
          }
        }

        agencyName = updateAgencyName
        users = updateAgencyUsers

        (
          agencyRenderer.map(_.setHtml).openOr(Noop) &
          updateCharts
        )
      }
    )
  }

  def dateFilterMonthDropdown = {
    SHtml.ajaxSelect(
      List(("All Months", "All Months")) ++ allShipmentsByMonthLabel.map(month => (month, month)),
      Full(monthDateFilter),
      (possibleMonth: String) => {
        monthDateFilter = possibleMonth
        (
          dateFilterTransform.map(_.setHtml).openOr(Noop) &
          updateCharts
        )
      }
    )
  }

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

    val allShipmentsByMonthValue = shipmentsByMonthSorted.values.toArray

    val totalActiveChartValues = {
      if (monthDateFilter == "All Months") {
        allShipmentsByMonthValue
      } else {
        val monthNumber = monthDateFilter match {
          case "Mar" => 3
          case "Apr" => 4
          case "May" => 5
          case "Jun" => 6
          case "Jul" => 7
          case "Aug" => 8
          case "Sep" => 9
          case "Oct" => 10
          case "Nov" => 11
          case "Dec" => 12
        }

        Array(shipmentsByMonthSorted(monthNumber))
      }
    }

    val totalActiveChartLabels = {
      if (monthDateFilter == "All Months") {
        allShipmentsByMonthLabel
      } else {
        Array(monthDateFilter)
      }
    }

    val activeUsersSignupByMonthValues = {
      if (monthDateFilter == "All Months") {
        activeUsersSignupByMonthValue
      } else {
        val monthNumber = monthDateFilter match {
          case "Mar" => 3
          case "Apr" => 4
          case "May" => 5
          case "Jun" => 6
          case "Jul" => 7
          case "Aug" => 8
          case "Sep" => 9
          case "Oct" => 10
          case "Nov" => 11
          case "Dec" => 12
        }

        Array(activeUsersSignupByMonthSorted(monthNumber))
      }
    }

    val activeUsersSignupByMonthLabels = {
      if (monthDateFilter == "All Months") {
        activeUsersSignupByMonthLabel
      } else {
        Array(monthDateFilter)
      }
    }

    (
      UpdateChartData("activeInactive", Array(activeUserCount, inactiveUserCount)) &
      UpdateChartData("totalActive", totalActiveChartValues, totalActiveChartLabels) &
      UpdateChartData("signup", activeUsersSignupByMonthValues, activeUsersSignupByMonthLabels)
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

    val currentMonthPets = currentMonthSignups.map(_.getPets).flatten
    val currentYearPets = currentYearSignups.map(_.getPets).flatten

    val ytdCommission = currentYearSignups.size * 12
    val mtdCommission = currentMonthPets.size * 12

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
      ".pet-status [class+]" #> findStatus(pet.status.get).toLowerCase
    }
  }

  def render = {
    ".overview [class+]" #> "current" &
    "#item-container" #> SHtml.idMemoize { agencyRender =>
      agencyRenderer = Full(agencyRender)

      snapShotBindings &
      ".store-name *" #> agencyName &
      "#choose-store" #> storeDropdown &
      ".update-data [onclick]" #> ajaxInvoke(() => updateCharts) &
      "#month-filter" #> dateFilterMonthDropdown &
      ".customer-list-container" #> SHtml.idMemoize { renderer =>
        dateFilterTransform = Full(renderer)

        ".customer-count *" #> usersWithName.size &
        ".export-customers [href]" #> (chosenAgency.map { realAgency =>
          AgencyOverview.exportAgencyCustomerMenu.calcHref(realAgency.agencyId.get.toString)
        }).getOrElse("#") &
        "tbody" #> usersWithName.sortWith(_.name < _.name).map { parent =>
          idMemoize { detailsRenderer =>

            ".user" #> {
              val subscription = parent.subscription.obj

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
  }
}
