package com.mypetdefense.service

import net.liftweb._
  import common._
  import mapper._
  import util._
  import util.Helpers._
  import net.liftweb.http._
    import js.JsCmds._

import com.mypetdefense.model._

import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import java.time.{LocalDate, ZoneId, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

object ReportingService extends Loggable {
  val signupCancelDateFormat = new SimpleDateFormat("MM/dd/yyyy")

  def currentDate = LocalDateTime.now()

  def yesterday = LocalDate.now(ZoneId.of("America/New_York")).atStartOfDay(ZoneId.of("America/New_York")).minusDays(1)

  def yesterdayStart = Date.from(LocalDate.now(ZoneId.of("America/New_York")).atStartOfDay(ZoneId.of("America/New_York")).minusDays(1).toInstant())

  def yesterdayEnd = Date.from(LocalDate.now(ZoneId.of("America/New_York")).atStartOfDay(ZoneId.of("America/New_York")).toInstant())
  
  def yearMonth = currentDate.format(DateTimeFormatter.ofPattern("MMMM yyyy", Locale.ENGLISH))
  
  def fileNameYearMonth = currentDate.format(DateTimeFormatter.ofPattern("MMMyyyy", Locale.ENGLISH))

  def fileNameMonthDayYear = currentDate.format(DateTimeFormatter.ofPattern("MM-dd-yyyy", Locale.ENGLISH))
  
  def year = currentDate.format(DateTimeFormatter.ofPattern("yyyy", Locale.ENGLISH))

  val monthHeaders = "January" :: "February" :: "March" :: "April" :: "May" :: "June" :: "July" :: "August" :: "September" :: "October" :: "November" :: "December" :: Nil

  val spacerRow = List(List(","))

  def getUsersForAgency(agencyName: String) = {
    val agency = Agency.find(By(Agency.name, agencyName))
    agency.map(_.customers.toList).openOr(Nil)
  }

  def getShipmentAmountPaid(shipment: Shipment) = {
    val amountPaid = tryo(shipment.amountPaid.get.toDouble).getOrElse(0D)
    val taxesPaid = tryo(shipment.taxPaid.get.toDouble).getOrElse(0D) 
    amountPaid - taxesPaid
  }

  def getProcessDateOfShipment(shipment: Shipment) = {
    shipment.dateProcessed.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()
  }

  def getMailedDateOfShipment(shipment: Shipment) = {
    tryo(shipment.dateShipped.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate())
  }

  def getStartDateOfSubscription(subscription: Subscription) = {
    tryo(signupCancelDateFormat.format(subscription.startDate.get)).openOr("")
  }

  def getCancelDateOfSubscription(subscription: Subscription) = {
    tryo(signupCancelDateFormat.format(subscription.cancellationDate.get)).openOr("")
  }

  def getCreatedDateOfUser(user: User) = {
    user.createdAt.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()
  }

  def getCreatedDateOfSubscription(subscription: Subscription) = {
    subscription.createdAt.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()
  }

  def getCancelledDateOfSubscription(subscription: Subscription) = {
    tryo(subscription.cancellationDate.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate())
  }

  def getNextShipDate(subscription: Subscription) = {
    subscription.nextShipDate.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()
  }

  def getSubscriptions(users: List[User]) = {
    users.map(_.subscription.toList).flatten
  }

  def getFirstShipments(subscriptions: List[Subscription]) = {
    val firstShipments = subscriptions.map(_.shipments.headOption).flatten

    firstShipments filter { shipment =>
      !getMailedDateOfShipment(shipment).isEmpty
    }
  }

  def filterMailedShipments (shipments: List[Shipment]) = {
    shipments filter { shipment =>
      val dateProcessed = getProcessDateOfShipment(shipment)

      val legacyShipment_? = dateProcessed.isBefore(LocalDate.parse("2018-01-01"))

      (!getMailedDateOfShipment(shipment).isEmpty || legacyShipment_?)
    }
  }

  def getShipments(subscriptions: List[Subscription]): List[Shipment] = {
    subscriptions.map(getShipments).flatten
  }

  def getShipments(subscription: Subscription) = {
    val shipments = subscription.shipments.toList

    filterMailedShipments(shipments)
  }

  def getPetCount(shipments: List[Shipment]) = {
    shipments.map(_.shipmentLineItems.toList).flatten.size
  }

  def totalSalesForShipments(shipments: List[Shipment]) = {
    shipments.map { shipment =>
        getShipmentAmountPaid(shipment)
    }.foldLeft(0D)(_+_)
  }

  def totalCommissionForSales(sales: Double) = {
    sales * 0.35
  }

  def findCurrentYearShipments(shipments: List[Shipment]) = {
    shipments.filter { shipment =>
      val mailedDate = getMailedDateOfShipment(shipment)
      
      mailedDate map { date =>
        (date.getYear == currentDate.getYear)
      } openOr(false)
    }
  }

  def getDateRange(month: String, year: Int = 2019) = {
    if (month == "") {
      currentDate
    } else {
      convertMonthToDate(month, year)
    }
  }

  def getYearOrCurrent(year: String) = {
    if (year == "") {
      currentDate.getYear
    } else {
      tryo(year.toInt).openOr(0)
    }
  }

  def findCurrentMonthShipments(shipments: List[Shipment], month: String = "") = {
    val date = getDateRange(month)

    shipments.filter { shipment =>
      val mailedDate = getMailedDateOfShipment(shipment)
      
      mailedDate map { mailDate =>
        (mailDate.getYear == 2019) &&
        (mailDate.getMonth == date.getMonth)
      } openOr(false)
    }
  }

  def findCurrentMonthProcessedShipments(shipments: List[Shipment], month: String = "", year: Int = 2019) = {
    val date = getDateRange(month)

    shipments.filter { shipment =>
      val processDate = getProcessDateOfShipment(shipment)

      (
        (processDate.getYear == year) &&
        (processDate.getMonth == date.getMonth)
      )
    }
  }

  def findCurrentMonthSubscriptions(subscriptions: List[Subscription]) = {
    subscriptions.filter { subscription =>
      getCreatedDateOfSubscription(subscription).getMonth == currentDate.getMonth
    }
  }

  def findActiveSubscriptionsFirstMonth(subscriptions: List[Subscription]) = {
    subscriptions.filter { subscription =>
      val createdDate = getCreatedDateOfSubscription(subscription)
      val cancellationDate = getCancelledDateOfSubscription(subscription)
      if (cancellationDate.isEmpty) {
        true
      } else {
        val createdDateMonth = createdDate.getMonth
        val createdDateYear = createdDate.getYear

        val cancelDateMonth = cancellationDate.map(_.getMonth)
        val cancelDateYear = cancellationDate.map(_.getYear)

        !(cancelDateYear.map(_ == createdDateYear).openOr(false) &&
          cancelDateMonth.map(_ == createdDateMonth).openOr(false)
        )
      }
    }
  }

  def findActiveSubscriptions(subscriptions: List[Subscription]) = {
    subscriptions.filter { subscription =>
      subscription.status != Status.Cancelled
    }
  }

  def findCustomersForAgent(customers: List[User], agentId: String) = {
    customers.filter(_.salesAgentId == agentId)
  }

  def convertMonthToDate(month: String, year: Int) = {
    val dateFormat = new SimpleDateFormat("MMMM yyyy")
    val monthDate = dateFormat.parse(s"$month $year")

    monthDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime()
  }

  def findSubscriptionMonthCancellations(subscriptions: List[Subscription], month: String = "") = {
    val date = getDateRange(month)

    subscriptions.filter { subscription =>
      val cancelDate = getCancelledDateOfSubscription(subscription)
      
      (
        cancelDate.map(_.getYear == 2019).openOr(false) &&
        cancelDate.map(_.getMonth == date.getMonth).openOr(false)
      )
    }
  }

  def findNewCustomersMonth(users: List[User], month: String = "", year: Int = 2019) = {
    val date = getDateRange(month)

    users filter { user =>
      val userCreatedDate = getCreatedDateOfUser(user)

      (
        (getCreatedDateOfUser(user).getYear == year) &&
        (getCreatedDateOfUser(user).getMonth == date.getMonth)
      )
    }
  }

  def findPayingUsers(users: List[User], month: String = "", year: Int = 2019) = {
    val date = getDateRange(month)

    users filter { user =>
      val userCreatedDate = getCreatedDateOfUser(user)

      !(
        (userCreatedDate.getYear == year) &&
        (userCreatedDate.getMonth == date.getMonth)
      )
    }
  }

  def findPaidShipments(subscriptions: List[Subscription]) = {
    val shipments = getShipments(subscriptions)
    
    shipments filter { shipment =>
      getShipmentAmountPaid(shipment) > 0.0
    }
  }

  def findCancelledSubscriptions(subscriptions: List[Subscription]) = {
    subscriptions.filter(_.status == Status.Cancelled)
  }

  def findCurrentYearPayingCancelledSubscriptions(subscriptions: List[Subscription]) = {
    subscriptions.filter { subscription =>
      val createdDate = getCreatedDateOfSubscription(subscription)
      val cancellationDate = getCancelledDateOfSubscription(subscription)
      if (cancellationDate.isEmpty) {
        false
      } else {
        val createdDateMonth = createdDate.getMonth
        val createdDateYear = createdDate.getYear

        val cancelDateMonth = cancellationDate.map(_.getMonth)
        val cancelDateYear = cancellationDate.map(_.getYear)

        (cancelDateYear.map(_ == currentDate.getYear).openOr(false) &&
          cancelDateMonth.map(_ != createdDateMonth).openOr(false)
        )
      }
    }
  }

  def findCurrentMonthCancelledSubscriptions(subscriptions: List[Subscription], month: String = "", year: Int = 2019) = {
    val date = getDateRange(month, year)

    subscriptions filter { subscription =>
      val cancelDate = getCancelledDateOfSubscription(subscription)

      cancelDate.map { cancelDate => 
        (cancelDate.getYear == year) &&
        (cancelDate.getMonth == date.getMonth)
      }.openOr(false)
    }
  }

  def generateCSV(csv: String, fileName: String) = {
    Some(new InMemoryResponse(
      csv.getBytes("UTF-8"),
      List(
        "Content-Type" -> "binary/octet-stream",
        "Content-Disposition" -> s"attachment; ${fileName}"
      ),
      Nil,
      200
    ))
  }

  def exportRawSales(name: String): Box[LiftResponse] = {
    val headers = "Year" :: "Month" :: "Mailed Date" :: "Customer Id" :: "Customer Name" :: "Amount" :: "Call Agent Id" :: "Commision" :: "Customer Status" :: Nil

    val csvRows: List[List[String]] = {
      for {
        agency <- Agency.find(By(Agency.name, name)).toList
        customer <- agency.customers.toList
        subscription <- customer.subscription
        shipment <- subscription.shipments.toList.sortBy(_.dateProcessed.get.getTime)
        mailedDate <- getMailedDateOfShipment(shipment)
      } yield {
        val amountPaid = getShipmentAmountPaid(shipment)
        val commision = amountPaid * .35 

        mailedDate.getYear.toString ::
        mailedDate.getMonth.toString ::
        mailedDate.toString ::
        customer.userId.toString ::
        customer.name ::
        s"$$${amountPaid}" ::
        customer.salesAgentId.get ::
        f"$$$commision%2.2f" ::
        subscription.status.toString ::
        Nil
      }
    }

    val csv = (List(headers) ++ csvRows).map(_.mkString(",")).mkString("\n")

    val fileName = s"salesData-${fileNameYearMonth}.csv"

    val file = "filename=\"" + fileName + "\""

    generateCSV(csv, file)
  }

  def exportCancellationData(name: String) = {
    val possibleAgency = Agency.find(By(Agency.name, name))
    val customers = possibleAgency.map(_.customers.toList).openOr(Nil)
    val cancelledCustomers = customers.filter(_.status == Status.Cancelled)

    val customerStatusBreakdown = customers.groupBy { customer =>
      customer.subscription.map(_.status.get)
    }

    val customerStatusTotals = customerStatusBreakdown.map { customer =>
      s"${customer._1.headOption.getOrElse("")} Users,${customer._2.size.toString}"
    }.toList.sorted.map(_ :: "," :: Nil)

    val topHeaders = List(
      "Cancellation Report",
      "YTD Cancels by Qty",
      "Avg Shipments",
      "Gross YTD Cancels by $",
      "Estimated Commission"
    )

    val currentYearSubscriptionCancels = {
      for {
        customer <- cancelledCustomers
        subscription <- customer.subscription
          if getCancelledDateOfSubscription(subscription).map(_.getYear == currentDate.getYear).openOr(false)
      } yield {
        subscription
      }
    }

    val currentYearCancelShipments = currentYearSubscriptionCancels.map(_.shipments.toList).flatten.filter { shipment => 
      !getMailedDateOfShipment(shipment).isEmpty
    }

    val averageShipmentsPerCancelByYear = currentYearCancelShipments.size.toDouble/currentYearSubscriptionCancels.size.toDouble

    val currentYearCancelTotal = totalSalesForShipments(currentYearCancelShipments)

    val yearCancelCommisionAmount = currentYearCancelTotal * .35

    val currentYearCancelSalesRow = List(f"Year To Date Totals,${currentYearSubscriptionCancels.size},$averageShipmentsPerCancelByYear%3.2f,$$$currentYearCancelTotal,$$$yearCancelCommisionAmount%3.2f")

    val currentMonthSubscriptionCancels = currentYearSubscriptionCancels.filter { subscription =>
      val cancelDate = getCancelledDateOfSubscription(subscription)
      cancelDate.map(_.getMonth == currentDate.getMonth).openOr(false)
    }

    val currentMonthCancelShipments = currentMonthSubscriptionCancels.map(_.shipments.toList).flatten.filter(shipment => !getMailedDateOfShipment(shipment).isEmpty)

    val averageShipmentsPerCancelByMonth = currentMonthCancelShipments.size.toDouble/currentMonthSubscriptionCancels.size.toDouble

    val currentMonthCancelTotal = totalSalesForShipments(currentMonthCancelShipments)

    val monthCancelCommisionAmount = currentMonthCancelTotal * .35

    val currentMonthCancelSalesRow = List(f"Month To Date Totals,${currentMonthSubscriptionCancels.size},$averageShipmentsPerCancelByMonth%3.2f,$$$currentMonthCancelTotal,$$$monthCancelCommisionAmount%3.2f")

    val headers = "Customer Id" :: "Start Month" :: "End Month" :: "Customer Status" :: "Shipment Count" :: "Total Gross Sales" :: "Total Commission" :: Nil

    val allCancellationRows: List[List[String]] = {
      for {
        customer <- customers.filter(_.status != Status.Active)
        subscription <- customer.subscription
        shipments = subscription.shipments.toList.sortBy(_.dateProcessed.get.getTime)
        firstShipment <- shipments.headOption
          if !getMailedDateOfShipment(firstShipment).isEmpty
        lastShipment <- shipments.lastOption
      } yield {
        val firstShipmentDate = getProcessDateOfShipment(firstShipment)
        val lastShipmentDate = getProcessDateOfShipment(lastShipment)
        val shipmentCount = shipments.size
        val totalGrossSales = shipments.map(getShipmentAmountPaid(_)).foldLeft(0D)(_+_)
        val totalCommission = totalGrossSales * .35

        customer.userId.toString ::
        firstShipmentDate.getMonth.toString ::
        lastShipmentDate.getMonth.toString::
        subscription.status.toString ::
        shipmentCount.toString ::
        s"$$${totalGrossSales}" ::
        f"$$$totalCommission%2.2f" ::
        Nil
      }
    }

    val csvRows = {
      customerStatusTotals ++
      spacerRow ++
      List(topHeaders) ++
      List(currentYearCancelSalesRow) ++
      List(currentMonthCancelSalesRow) ++
      spacerRow ++
      List(headers) ++
      allCancellationRows
    }.map(_.mkString(",")).mkString("\n")

    val fileName = s"cancellations-${fileNameYearMonth}.csv"

    val file = "filename=\"" + fileName + "\""

    generateCSV(csvRows, file)
  }

  def exportTotalSales(name: String): Box[LiftResponse] = {
    val allShipments = {
      for {
        agency <- Agency.find(By(Agency.name, name)).toList
        customer <- agency.customers.toList
        subscription <- customer.subscription
        shipment <- subscription.shipments.toList.sortBy(_.dateProcessed.get.getTime)
      } yield {
        shipment
      }
    }

    val currentYearShipments = allShipments.filter { shipment =>
      val processDate = getProcessDateOfShipment(shipment)

      processDate.getYear == currentDate.getYear
    }

    val currentYearTotal = totalSalesForShipments(currentYearShipments)

    val currentYearSalesRow = s"${year},$$${currentYearTotal}"

    val shipmentsByMonth = allShipments.groupBy { shipment =>
      val processDate = getProcessDateOfShipment(shipment)

      processDate.getMonth
    }

    val totalSalesByMonth = shipmentsByMonth.map { case (month, shipments) =>
      (month, s"$$${totalSalesForShipments(shipments)}")
    }

    val salesMonthsHeaders = totalSalesByMonth.map(_._1).mkString(",")
    val salesMonthsTotals = totalSalesByMonth.map(_._2).mkString(",")

    println("===============")
    println(currentYearSalesRow)
    println("========= year ^")
    println("===============")
    println(salesMonthsHeaders)
    println(salesMonthsTotals)
    println("========= month ^")

    Empty

  }

  def exportMonthToDateSales(name: String): Box[LiftResponse] = {
    val allShipments = {
      for {
        agency <- Agency.find(By(Agency.name, name)).toList
        customer <- agency.customers.toList
        subscription <- customer.subscription
        shipment <- subscription.shipments.toList.sortBy(_.dateProcessed.get.getTime)
      } yield {
        shipment
      }
    }

    val totalUsers = Agency.find(By(Agency.name, name)).headOption.map(_.customers.toList).getOrElse(Nil)

    val currentYearShipments = allShipments.filter { shipment =>
      val processDate = getProcessDateOfShipment(shipment)

      processDate.getYear == currentDate.getYear
    }

    val currentYearTotal = totalSalesForShipments(currentYearShipments)

    val yearCommisionAmount = currentYearTotal * .35

    val currentYearMTDSalesRow = List(f"Year To Date Totals,${totalUsers.size}")

    val usersByCurrentMonth = totalUsers.filter { user =>
      (getCreatedDateOfUser(user).getMonth == currentDate.getMonth) && (getCreatedDateOfUser(user).getYear == currentDate.getYear)
    }

    val shipmentsByCurrentMonth = allShipments.filter { shipment =>
      val mailedDate = getMailedDateOfShipment(shipment)

      mailedDate.map(_.getMonth == currentDate.getMonth).openOr(false) && 
      mailedDate.map(_.getYear == currentDate.getYear).openOr(false)
    }

    val currentMonthTotal = totalSalesForShipments(shipmentsByCurrentMonth)

    val monthCommisionAmount = currentMonthTotal * .35

    val currentMonthSalesRow = List(f"Month To Date Totals,${usersByCurrentMonth.size}")

    val topHeaders = List(
      "Sales Report",
      "New Sales by Qty"
    )

    val shipmentMonthByAgent = shipmentsByCurrentMonth.groupBy { shipment =>
      val user = { 
        for {
          subscription <- shipment.subscription.obj
          user <- subscription.user.obj
        } yield {
          user
        }
      }

      user.map(_.salesAgentId.get).openOr("")
    }

    val usersMonthByAgent = usersByCurrentMonth.groupBy { user =>
      user.salesAgentId.get
    }

    val agentSalesData: List[List[String]] = usersMonthByAgent.map { case (agentId, users) =>
      List(f"$agentId,${users.size}")
    }.toList

    val agentHeaders = List(
      "MTD Sales by Rep",
      "New MTD Sales by Qty"
    )


    val allSalesForMonthData: String = shipmentMonthByAgent.map { case (agentId, shipments) =>
      shipments.map { shipment =>
        val customerId = { 
          for {
            subscription <- shipment.subscription.obj
            user <- subscription.user.obj
          } yield {
              user.userId.toString
          }
        }.openOr("-")
        
        val shipmentTotal = getShipmentAmountPaid(shipment)
        val commisionTotal = shipmentTotal * .35

        f"$agentId,$customerId,${getProcessDateOfShipment(shipment)}"
      }.mkString("\n")
    }.mkString("\n")

    val allSalesHeaders = List(
      "Rep ID",
      "Customer ID",
      "Date",
      "Value",
      "Estimated Commission"
    )
    
    val csvRows = {
      List(topHeaders) ++
      List(currentYearMTDSalesRow) ++
      List(currentMonthSalesRow) ++
      spacerRow ++
      List(agentHeaders) ++
      agentSalesData ++
      spacerRow ++
      List(allSalesHeaders)
    }.map(_.mkString(",")).mkString("\n") + "\n" + allSalesForMonthData

    val fileName = s"month-to-date-salesData-${fileNameMonthDayYear}.csv"

    val file = "filename=\"" + fileName + "\""

    generateCSV(csvRows, file)
  }

  def exportAgencyMtdYtdSales(name: String): Box[LiftResponse] = {
    val newCustomerHeaders = List(
      "",
      "New Customers",
      "Cancellations",
      "Net New Customers",
      "New Customer Shipments",
      "New Pets Shipped"
    )

    val payingCustomerHeaders = List(
      "",
      "Paying Customers",
      "Cancellations",
      "Paid Shipments",
      "Paid Pets Shipped",
      "Gross Sales",
      "Estimated Comission"
    )

    val agentNewCustomerHeaders = List(
      "MTD New Sales by Agent",
      "Net New Customers",
      "Cancellations",
      "Total Shipments",
      "Total Pets"
    )

    val agentPayingCustomerHeaders = List(
      "MTD Paying Sales by Agent",
      "Paying Customers",
      "Cancellations",
      "Paid Shipments",
      "Paid Pets Shipped",
      "Gross Sales",
      "Estimated Commission"
    )

    val totalUsers = Agency.find(By(Agency.name, name)).map(_.customers.toList).getOrElse(Nil)

    val newUsersYear = totalUsers.filter { user =>
      getCreatedDateOfUser(user).getYear == currentDate.getYear
    }

    val newUsersMonth = newUsersYear.filter { user =>
      getCreatedDateOfUser(user).getMonth == currentDate.getMonth
    }

    val newUsersYearSubscriptions = getSubscriptions(newUsersYear)
    
    val newUsersYearShipments = getFirstShipments(newUsersYearSubscriptions)
    val newUsersMonthShipments = findCurrentMonthShipments(newUsersYearShipments)

    val newUsersYearShippedPetCount = getPetCount(newUsersYearShipments)
    val newUsersMonthShippedPetCount = getPetCount(newUsersMonthShipments)

    val newUsersYearSubscriptionFirstMonthActive = findActiveSubscriptionsFirstMonth(newUsersYearSubscriptions)
    val newUsersYearSubscriptionCancelled = newUsersYearSubscriptions diff newUsersYearSubscriptionFirstMonthActive

    val newUsersMonthSubscriptionActive = findCurrentMonthSubscriptions(newUsersYearSubscriptionFirstMonthActive)
    val newUsersMonthSubscriptionCancelled = findCurrentMonthSubscriptions(newUsersYearSubscriptionCancelled)

    val mtdNewCustomers = List(s"Month to Date,${newUsersMonth.size},${newUsersMonthSubscriptionCancelled.size},${newUsersMonthSubscriptionActive.size},${newUsersMonthShipments.size},${newUsersMonthShippedPetCount}")

    val ytdNewCustomers = List(s"Year to Date,${newUsersYearSubscriptions.size},${newUsersYearSubscriptionCancelled.size},${newUsersYearSubscriptionFirstMonthActive.size},${newUsersYearShipments.size},${newUsersYearShippedPetCount}")

    val allPayingCustomers = findPayingUsers(totalUsers)

    val allPayingSubscriptions = getSubscriptions(allPayingCustomers)
    val allPayingActiveSubscriptions = findActiveSubscriptions(allPayingSubscriptions)
    
    val allPayingCancelledSubscriptions = findCancelledSubscriptions(allPayingSubscriptions)
    val paidSubscriptionYearCancelled = findCurrentYearPayingCancelledSubscriptions(allPayingCancelledSubscriptions)
    val paidSubscriptionMonthCancelled = findCurrentMonthCancelledSubscriptions(allPayingCancelledSubscriptions)

    val allPaidShipments = findPaidShipments(allPayingSubscriptions)
    
    val paidYearShipments = findCurrentYearShipments(allPaidShipments)
    val paidMonthShipments = findCurrentMonthShipments(allPaidShipments)

    val paidYearPetsShippedCount = getPetCount(paidYearShipments)
    val paidMonthPetsShippedCount = getPetCount(paidMonthShipments)

    val paidYearGrossSales = totalSalesForShipments(paidYearShipments)
    val paidMonthGrossSales = totalSalesForShipments(paidMonthShipments)

    val paidYearCommission = totalCommissionForSales(paidYearGrossSales)
    val paidMonthCommission = totalCommissionForSales(paidMonthGrossSales)

    val mtdPayingCustomers = List(f"Month to Date,${allPayingActiveSubscriptions.size},${paidSubscriptionMonthCancelled.size},${paidMonthShipments.size},$paidMonthPetsShippedCount,$$$paidMonthGrossSales%2.2f,$$$paidMonthCommission%2.2f")

    val ytdPayingCustomers = List(f"Year to Date,${allPayingActiveSubscriptions.size},${paidSubscriptionYearCancelled.size},${paidYearShipments.size},$paidYearPetsShippedCount,$$$paidYearGrossSales%2.2f,$$$paidYearCommission%2.2f")

    val paidMonthShipmentByAgent = paidMonthShipments.groupBy { shipment =>
      val user = { 
        for {
          subscription <- shipment.subscription.obj
          user <- subscription.user.obj
        } yield {
          user
        }
      }

      user.map(_.salesAgentId.get).openOr("")
    }

    val paidCustomersByAgent = paidMonthShipmentByAgent.map { case (agentId, shipments) =>

      val shipmentsPetCount = getPetCount(shipments)
      val shipmentsTotal = totalSalesForShipments(shipments)
      val commisionTotal = totalCommissionForSales(shipmentsTotal)

      val activePayingUsers = allPayingActiveSubscriptions.map(_.user.obj.toList).flatten

      val cancelledMonthPaidUsers = paidSubscriptionMonthCancelled.map(_.user.obj.toList).flatten

      val activePayingAgentCustomer = findCustomersForAgent(activePayingUsers, agentId)
      val cancelledPayingAgentCustomer = findCustomersForAgent(cancelledMonthPaidUsers, agentId)

      List(f"$agentId,${activePayingAgentCustomer.size},${cancelledPayingAgentCustomer.size},${shipments.size},$shipmentsPetCount,$$$shipmentsTotal%2.2f,$$$commisionTotal%2.2f")
    }

    val newMonthShipmentByAgent = newUsersMonthShipments.groupBy { shipment =>
      val user = { 
        for {
          subscription <- shipment.subscription.obj
          user <- subscription.user.obj
        } yield {
          user
        }
      }

      user.map(_.salesAgentId.get).openOr("")
    }

    val newCustomersByAgent = newMonthShipmentByAgent.map { case (agentId, shipments) =>

      val shipmentsPetCount = getPetCount(shipments)

      val activeNewUsers = newUsersMonthSubscriptionActive.map(_.user.obj.toList).flatten

      val cancelledNewUsers = newUsersMonthSubscriptionCancelled.map(_.user.obj.toList).flatten

      val activeNewAgentCustomer = findCustomersForAgent(activeNewUsers, agentId)
      val cancelledNewAgentCustomer = findCustomersForAgent(cancelledNewUsers, agentId)

      List(f"$agentId,${activeNewAgentCustomer.size},${cancelledNewAgentCustomer.size},${shipments.size},$shipmentsPetCount")
    }

    val csvRows = {
      List(newCustomerHeaders) ++
      List(mtdNewCustomers) ++
      List(ytdNewCustomers) ++
      spacerRow ++
      List(payingCustomerHeaders) ++
      List(mtdPayingCustomers) ++
      List(ytdPayingCustomers) ++
      spacerRow ++
      List(agentPayingCustomerHeaders) ++
      paidCustomersByAgent ++
      spacerRow ++
      List(agentNewCustomerHeaders) ++
      newCustomersByAgent
    }.map(_.mkString(",")).mkString("\n")

    val fileName = s"${name}-mtd-ytd-${fileNameMonthDayYear}.csv"

    val file = "filename=\"" + fileName + "\""

    generateCSV(csvRows, file)
  }

  def findPreviousWeekSales(agency: String) = {
    val totalUsers = Agency.find(By(Agency.name, agency)).map(_.customers.toList).getOrElse(Nil)

    val lastWeekNewUsers = totalUsers.filter { user =>
      val createdDayOfYear = getCreatedDateOfUser(user).getDayOfYear
      val currentDayOfYear = currentDate.getDayOfYear
      
      (currentDayOfYear - createdDayOfYear) < 8
    }

    val lastWeekNewSubscriptions = lastWeekNewUsers.map(_.getSubscription).flatten

    val lastWeekNewSubscriptionCancels = lastWeekNewSubscriptions filter { subscription =>
      val cancellationDate = getCancelledDateOfSubscription(subscription)
      !cancellationDate.isEmpty
    }
  }

  def findYesterdayNewSales = {
     User.findAll(
       By_>(User.createdAt, yesterdayStart),
       By_<(User.createdAt, yesterdayEnd)
     )
  }

  def yesterdayShipments = {
     val yesterdayShipments = Shipment.findAll(
       By_>(Shipment.dateShipped, yesterdayStart),
       By_<(Shipment.dateShipped, yesterdayEnd)
     )

     val paidShipments = yesterdayShipments.filter(_.amountPaid.get != "0")
     val newShipments = yesterdayShipments diff paidShipments
     val paidGrossSales = totalSalesForShipments(paidShipments)
    
     (newShipments.size, paidShipments.size, paidGrossSales)
  }

  def yesterdayCancels = {
    Subscription.findAll(
      By_>(Subscription.cancellationDate, yesterdayStart),
      By_<(Subscription.cancellationDate, yesterdayEnd)
    )
  }

  def findYesterdaySalesByAgent: List[(String, Int)] = {
    val totalUsers = Agency.findAll(NotBy(Agency.name, "My Pet Defense")).flatMap(_.customers.toList)

    val newUsersYesterday = totalUsers.filter { user =>
      val createdDate = getCreatedDateOfUser(user)
      
      val createdDateDay = createdDate.getDayOfMonth
      val createdDateMonth = createdDate.getMonth
      val createdDateYear = createdDate.getYear

      val yesterdayDay = yesterday.getDayOfMonth
      val yesterdayMonth = yesterday.getMonth
      val yesterdayYear = yesterday.getYear

      (
        (createdDateDay == yesterdayDay) &&
        (createdDateMonth == yesterdayMonth) &&
        (createdDateYear == yesterdayYear)
      )
    }

    newUsersYesterday.groupBy(_.salesAgentId.get).map { case (agentId, users) =>
      val pets = users.flatMap(_.pets.toList)
      (agentId -> pets.size)
    }.toList.sortBy(_._1)
  }

  def findMTDSalesByAgent: List[(String, Int)] = {
    val yesterday = LocalDate.now(ZoneId.of("America/New_York")).minusDays(1)
   val totalUsers = Agency.findAll(NotBy(Agency.name, "My Pet Defense")).flatMap(_.customers.toList)

    val newUsersThisMonth = totalUsers.filter { user =>
      val createdDayDate = getCreatedDateOfUser(user)
      val yesterdayDayOfYear = currentDate.getDayOfYear - 1
      
      (
        createdDayDate.getYear == yesterday.getYear &&
        createdDayDate.getMonth == yesterday.getMonth
      )
    }

    newUsersThisMonth.groupBy(_.salesAgentId.get).map { case (agentId, users) =>
      val pets = users.flatMap(_.pets)
      (agentId -> pets.size)
    }.toList.sortBy(_._1)
  }

  def exportAgencyMonthSales(name: String, month: String, possibleYear: String): Box[LiftResponse] = {
    val year = getYearOrCurrent(possibleYear)
    val totalUsers = Agency.find(By(Agency.name, name)).map(_.customers.toList).getOrElse(Nil)

    val totalSubscriptions = getSubscriptions(totalUsers)
    val totalCancelledSubscriptions = findCurrentMonthCancelledSubscriptions(totalSubscriptions, month, year)

    val newUsersMonth = findNewCustomersMonth(totalUsers, month, year)
    val netNewUsersMonth = newUsersMonth.filter(_.status != Status.Cancelled)
    val payingUsers = findPayingUsers(totalUsers, month, year)

    val allPayingSubscriptions = getSubscriptions(payingUsers)
    val allPayingActiveSubscriptions = findActiveSubscriptions(allPayingSubscriptions)

    val allPaidShipments = findPaidShipments(allPayingSubscriptions)
    val paidMonthShipments = findCurrentMonthProcessedShipments(allPaidShipments, month, year)
    val paidMonthPetsShippedCount = getPetCount(paidMonthShipments)
    val paidMonthGrossSales = totalSalesForShipments(paidMonthShipments)
    val paidMonthCommission = totalCommissionForSales(paidMonthGrossSales)
    val discountCount = (paidMonthPetsShippedCount * 12.99) - paidMonthGrossSales

    val csvRows = {
      List(List("Time Period", s"$month $year")) ++
      spacerRow ++
      List(List("Net New Users", netNewUsersMonth.size)) ++
      List(List("Total Cancellations", totalCancelledSubscriptions.size)) ++
      List(List("Paid Shipments", paidMonthShipments.size)) ++
      List(List("Paid Pets", paidMonthPetsShippedCount)) ++
      List(List("Multi Pet Discount", f"$$$discountCount%2.0f")) ++
      List(List("Gross Sales", f"$$$paidMonthGrossSales%2.2f")) ++
      List(List("Estimated Commission", f"$$$paidMonthCommission%2.2f"))
    }.map(_.mkString(",")).mkString("\n")

    val fileName = s"${name}-${month}-${year}-sales-summary.csv"

    val file = "filename=\"" + fileName + "\""

    generateCSV(csvRows, file)
  }

  def exportSameDayCancels(name: String): Box[LiftResponse] = {
    val headers = "Month" :: "Cancel Count" :: Nil

    val csvRows: List[List[String]] = {
      for {
        agency <- Agency.find(By(Agency.name, name)).toList
        customers = agency.customers.toList
        subscriptions = customers.map(_.getSubscription.toList).flatten
        cancelsByMonth <- sameDayCancelsByMonth(subscriptions)
      } yield {
        cancelsByMonth._1.toString ::
        cancelsByMonth._2.toString ::
        Nil
      }
    }

    val csv = (List(headers) ++ csvRows).map(_.mkString(",")).mkString("\n")

    val fileName = s"sameDayCancels-${LocalDate.now()}.csv"

    val file = "filename=\"" + fileName + "\""

    generateCSV(csv, file)
  }

  def cancelsByShipment(subscriptions: List[Subscription]) = {
    def findCancellationShipmentSize(subscriptions: List[Subscription]) = {
      subscriptions.map { subscription =>
        val shipments = subscription.shipments.toList

        val mailedShipments = filterMailedShipments(shipments)

        mailedShipments.size
      }
    }

    val cancellations = subscriptions.filter(_.status.get == Status.Cancelled)
    
    val cancellationCount: Double = cancellations.size

    val cancellationShipments = findCancellationShipmentSize(cancellations)
    
    val cancellationTimes = (List(0, 1, 2, 3, 4, 5).map { count =>
      val totalForCount = cancellationShipments.filter(_ == count).size
      
      (count.toString, totalForCount)
    }) ++ List(("6+", cancellationShipments.filter(_ >= 6).size))

    cancellationTimes
  }

  def sameDayCancelsByMonth(subscriptions: List[Subscription]) = {
    def findSameDayCancellations(subscriptions: List[Subscription]) = {
      subscriptions.filter { subscription =>
        val shipments = subscription.shipments.toList

        val mailedShipments = filterMailedShipments(shipments)

        mailedShipments.size == 0
      }
    }

    val cancellations = subscriptions.filter(_.status.get == Status.Cancelled)
    
    val sameDayCancels = findSameDayCancellations(cancellations)

    val cancelsByMonth = sameDayCancels.groupBy { subscription =>
      getCreatedDateOfSubscription(subscription).getMonth
    }

    cancelsByMonth.map { case (month, subscriptions) =>
      (month, subscriptions.size)
    }
  }

  def exportAgencyCustomers(rawAgencyId: String): Box[LiftResponse] = {
    val headers = "Name" :: "Status" :: "Shipment Count" :: "Signup Date" :: "Cancellation Date" :: Nil

    var agencyName = ""

    val csvRows: List[List[String]] = {
      for {
        agencyId <- tryo(rawAgencyId.toLong).toList
        agency <- Agency.find(By(Agency.agencyId, agencyId)).toList
        customer <- agency.customers.toList
        subscription <- customer.subscription.toList
      } yield {
        agencyName = agency.name.get

        val status = {
          if (subscription.status.get == Status.Active)
            "Active"
          else 
            "Inactive"
        }

        val startDate = getStartDateOfSubscription(subscription)
        val cancelDate = getCancelDateOfSubscription(subscription)

        customer.name ::
        status ::
        subscription.shipments.toList.size.toString ::
        startDate ::
        cancelDate ::
        Nil
      }
    }

    val csv = (List(headers) ++ csvRows).map(_.mkString(",")).mkString("\n")

    val fileName = s"${agencyName}-customers-${fileNameMonthDayYear}.csv"

    val file = "filename=\"" + fileName + "\""

    generateCSV(csv, file)
  }
}
