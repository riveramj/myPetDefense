package com.mypetdefense.snippet
package agency

import java.text.SimpleDateFormat
import java.time.{LocalDate, LocalDateTime, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.service.ReportingService
import com.mypetdefense.snippet.admin.UpdateChartData
import com.mypetdefense.util.SecurityContext
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.mapper.By
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._

import scala.collection.immutable.ListMap
import scala.xml.Elem

object AgencyOverview extends Loggable {
  import net.liftweb.sitemap._
  import Loc._
  import com.mypetdefense.util.Paths._

  def agencyCusomterExport: Box[LiftResponse] = {
    exportAgencyCustomerMenu.currentValue flatMap { agency =>
      ReportingService.exportAgencyCustomers(agency)
    }
  }

  val menu: Menu.Menuable = Menu.i("Agency Overview") / "agency" / "agency-overview" >>
    adminUser >>
    loggedIn

  val exportAgencyCustomerMenu: Menu.ParamMenuable[String] = Menu.param[String](
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
  val currentUser: Box[User] = SecurityContext.currentUser
  val signupCancelDateFormat = new SimpleDateFormat("MM/dd/yyyy")
  val chartDateFormat        = new SimpleDateFormat("MM")
  val chartDateFormatName    = new SimpleDateFormat("MMM")

  val allUsers: List[User] = User.findAll(
    By(User.userType, UserType.Parent)
  )

  val agencies: List[Agency] = {
    if (currentUser.map(_.petlandData_?).openOr(false)) {
      Agency.findAll(By(Agency.petlandStore, true))
    } else {
      currentUser.flatMap(_.agency.obj).toList
    }
  }

  var currentParent: Box[User]                     = Empty
  var dateFilterTransform: Box[IdMemoizeTransform] = Empty
  var agencyRenderer: Box[IdMemoizeTransform]      = Empty

  def updateAgencyName: String = {
    chosenAgency.map(_.name.get).getOrElse("All Stores")
  }

  def updateAgencyUsers: List[User] = {
    if (chosenAgency.isEmpty) {
      agencies.map { agency => allUsers.filter(_.referer.obj == Full(agency)) }.flatten
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

  var agencyName: String = updateAgencyName
  var users: List[User]  = updateAgencyUsers

  def allSubscriptions: List[Subscription] = users.flatMap(_.subscription.obj)
  def allShipments: List[Shipment]         = allSubscriptions.flatMap(_.shipments.toList)

  def usersByStatus: Map[Status.Value, List[User]] = users.groupBy(_.status.get)
  def activeUsers: List[User]                      = tryo(usersByStatus(Status.Active)).openOr(Nil)
  def activeUserCount: Int                         = activeUsers.size
  def inactiveUserCount: Int                       = users.size - activeUserCount

  def activeUserSubscription: List[Subscription] = users.flatMap(_.subscription.obj)
  def activeShipments: List[Shipment]            = activeUserSubscription.flatMap(_.shipments.toList)

  def usersWithName: List[User] = users.map(updateUserName)

  def shipmentsByMonth: Map[Int, Int] =
    allShipments
      .map(_.dateProcessed.get)
      .map(chartDateFormat.format)
      .map(toInt)
      .groupBy(identity)
      .mapValues(_.size)

  def adjustedShipmentsByMonth: Map[Int, Int] = shipmentsByMonth + (12 -> activeUserCount)

  def shipmentsByMonthSorted: ListMap[Int, Int] =
    ListMap(adjustedShipmentsByMonth.toSeq.sortBy(_._1): _*)

  def allShipmentsByMonthLabel: Array[String] = shipmentsByMonthSorted.keys.toArray.map { month =>
    chartDateFormatName.format(chartDateFormat.parse(month.toString))
  }

  var monthDateFilter       = "All Months"
  var storeIdFilter: String = chosenAgency.map(_.agencyId.get).openOr(-1L).toString

  def storeDropdown: xml.Elem = {
    val agencyList = {
      if (agencies.size == 1) {
        agencies.map(agency => (agency.agencyId.get.toString, agency.name.get))
      } else {
        List((-1L.toString, "All Stores")) ++ agencies.map(agency =>
          (agency.agencyId.get.toString, agency.name.get)
        )
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

  def dateFilterMonthDropdown: Elem = {
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

  def findStatus(status: Status.Value): String = {
    if (status == Status.Active)
      "Active"
    else
      "Inactive"
  }

  def updateUserName(parent: User): User = {
    if (parent.status == Status.Cancelled) {
      val cancelledUser = findCancelledUserName(parent)
      parent
        .firstName(cancelledUser.map(_.firstName.get).openOr(""))
        .lastName(cancelledUser.map(_.lastName.get).openOr(""))
    } else {
      parent
    }
  }

  def findCancelledUserName(parent: User): Box[CancelledUser] = {
    CancelledUser.find(By(CancelledUser.user, parent.userId.get))
  }

  def currentDate: LocalDateTime = LocalDateTime.now()

  def getDateRange(month: String): LocalDateTime = {
    if (month == "") {
      currentDate
    } else {
      convertMonthToDate(month)
    }
  }

  def getProcessDateOfShipment(shipment: Shipment): LocalDate = {
    shipment.dateProcessed.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()
  }

  def findCurrentMonthShipment(shipment: Shipment, month: String = ""): Boolean = {
    val date = getDateRange(month)

    val processedDate = getProcessDateOfShipment(shipment)

    (
      (processedDate.getYear == date.getYear) &&
      (processedDate.getMonth == date.getMonth)
    )
  }

  def findCurrentYearSubscriptionSignup(subscription: Subscription): Boolean = {
    val signupDate =
      subscription.startDate.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()

    signupDate.getYear == currentDate.getYear
  }

  def findCurrentMonthSubscriptionSignup(subscription: Subscription): Boolean = {
    val signupDate =
      subscription.startDate.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()

    signupDate.getMonth == currentDate.getMonth
  }

  def convertMonthToDate(month: String): LocalDateTime = {
    val dateFormat = new SimpleDateFormat("MMMM yyyy")
    val monthDate  = dateFormat.parse(s"$month 2018") //TODO: dynanmic year

    monthDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime()
  }

  def updateCharts: JsCmd = {
    val activeUserSignupDates = activeUserSubscription.map(_.startDate.get)

    val activeUsersSignupByMonth = activeUserSignupDates
      .map(chartDateFormat.format)
      .map(toInt)
      .groupBy(identity)
      .mapValues(_.size)

    val activeUsersSignupByMonthSorted = ListMap(activeUsersSignupByMonth.toSeq.sortBy(_._1): _*)

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

  def snapShotBindings: CssSel = {
    val currentMonthSubscriptionShipments = {
      for {
        subscription <- allSubscriptions
        shipment     <- subscription.shipments.toList
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
    val currentYearPets  = currentYearSignups.map(_.getPets).flatten

    val ytdCommission = currentYearSignups.size * 12
    val mtdCommission = currentMonthPets.size * 12

    ".mtd-shipments *" #> currentMonthSubscriptionShipments.size &
      ".mtd-commission-earned *" #> s"$$$mtdCommission" &
      ".ytd-commission-earned *" #> s"$$$ytdCommission"
  }

  def petBindings: CssSel = {
    val parent = currentParent
    val pets   = parent.map(_.pets.toList).openOr(Nil)

    ".pet" #> pets.map { pet =>
      ".pet-name *" #> pet.name.get &
        ".pet-status *" #> findStatus(pet.status.get) &
        ".pet-status [class+]" #> findStatus(pet.status.get).toLowerCase
    }
  }

  def render: CssSel = {
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
                    ".info [class+]" #> { if (currentParent.isEmpty) "" else "expanded" } &
                    "^ [class+]" #> { if (currentParent.isEmpty) "" else "expanded" } &
                    petBindings
                }
              }
          }
      }
  }
}
