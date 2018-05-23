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

object ReportingService extends Loggable {
  def currentDate = LocalDateTime.now()
  
  def yearMonth = currentDate.format(DateTimeFormatter.ofPattern("MMMM yyyy", Locale.ENGLISH))
  
  def fileNameYearMonth = currentDate.format(DateTimeFormatter.ofPattern("MMMyyyy", Locale.ENGLISH))

  def fileNameMonthDayYear = currentDate.format(DateTimeFormatter.ofPattern("MM-dd-yyyy", Locale.ENGLISH))
  
  def year = currentDate.format(DateTimeFormatter.ofPattern("yyyy", Locale.ENGLISH))

  val monthHeaders = "January" :: "February" :: "March" :: "April" :: "May" :: "June" :: "July" :: "August" :: "September" :: "October" :: "November" :: "December" :: Nil

  val spacerRow = List(List(","))

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

  def getCreatedDateOfUser(user: User) = {
    user.createdAt.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()
  }

  def getCreatedDateOfSubscription(subscription: Subscription) = {
    subscription.createdAt.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()
  }

  def getCancelledDateOfSubscription(subscription: Subscription) = {
    tryo(subscription.cancellationDate.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate())
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

  def getShipments(subscriptions: List[Subscription]) = {
    val allShipments = subscriptions.map(_.shipments.toList).flatten

    allShipments filter { shipment =>
      !getMailedDateOfShipment(shipment).isEmpty
    }
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

  def findCurrentMonthShipments(shipments: List[Shipment]) = {
    shipments.filter { shipment =>
      val mailedDate = getMailedDateOfShipment(shipment)
      
      mailedDate map { date =>
        (date.getYear == currentDate.getYear) &&
        (date.getMonth == currentDate.getMonth)
      } openOr(false)
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
      (subscription.status == Status.Active) || (subscription.status == Status.UserSuspended)
    }
  }

  def findCustomersForAgent(customers: List[User], agentId: String) = {
    customers.filter(_.salesAgentId == agentId)
  }

  def findPayingUsers(users: List[User]) = {
    users filter { user =>
      val userCreatedDate = getCreatedDateOfUser(user)

      !(
        (userCreatedDate.getYear == currentDate.getYear) &&
        (userCreatedDate.getMonth == currentDate.getMonth)
      )
    }
  }

  def findPaidShipments(subscriptions: List[Subscription]) = {
    val shipments = getShipments(subscriptions)
    
    shipments filter { shipment =>
      getShipmentAmountPaid(shipment) > 0.0
    }
  }

  def findCurrentYearCancelledSubscriptions(subscriptions: List[Subscription]) = {
    subscriptions filter { subscription =>
      val cancelDate = getCancelledDateOfSubscription(subscription)

      cancelDate.map(_.getYear == currentDate.getYear).openOr(false)
    }
  }

  def findCurrentMonthCancelledSubscriptions(subscriptions: List[Subscription]) = {
    subscriptions filter { subscription =>
      val cancelDate = getCancelledDateOfSubscription(subscription)

      cancelDate.map { cancelDate => 
        (cancelDate.getYear == currentDate.getYear) &&
        (cancelDate.getMonth == currentDate.getMonth)
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
    val headers = "Year" :: "Month" :: "Date" :: "Customer Id" :: "Customer Name" :: "Amount" :: "Call Agent Id" :: "Commision" :: "Customer Status" :: Nil

    val csvRows: List[List[String]] = {
      for {
        agency <- Agency.find(By(Agency.name, name)).toList
        customer <- agency.customers.toList
        subscription <- customer.subscription
        shipment <- subscription.shipments.toList.sortBy(_.dateProcessed.get.getTime)
      } yield {
        val processDate = getProcessDateOfShipment(shipment)

        val amountPaid = getShipmentAmountPaid(shipment)
        val commision = amountPaid * .35 

        processDate.getYear.toString ::
        processDate.getMonth.toString ::
        processDate.toString ::
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

    /*
    val processDate = getProcessDateOfShipment(shipment)

    val amountPaid = getShipmentAmountPaid(shipment)
    val commision = amountPaid * .35 

    processDate.getYear.toString ::
    processDate.getMonth.toString ::
    processDate.toString ::
    customer.userId.toString ::
    customer.name ::
    s"$$${amountPaid}" ::
    customer.salesAgentId.get ::
    f"$$$commision%2.2f" ::
    subscription.status.toString ::
    Nil

    val csv = (List(headers) ++ csvRows).map(_.mkString(",")).mkString("\n")

    val fileName = s"salesData-${fileNameYearMonth}.csv"

    val file = "filename=\"" + fileName + "\""

    generateCSV(csv, file)
    */

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
    
    val allPayingCancelledSubscriptions = allPayingSubscriptions diff allPayingActiveSubscriptions 
    val paidSubscriptionYearCancelled = findCurrentYearCancelledSubscriptions(allPayingCancelledSubscriptions)
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
}
