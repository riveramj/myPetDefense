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
  val currentDate = LocalDateTime.now()
  
  val yearMonth = currentDate.format(DateTimeFormatter.ofPattern("MMMM yyyy", Locale.ENGLISH))
  
  val fileNameYearMonth = currentDate.format(DateTimeFormatter.ofPattern("MMMyyyy", Locale.ENGLISH))

  val fileNameMonthDayYear = currentDate.format(DateTimeFormatter.ofPattern("MM-dd-yyyy", Locale.ENGLISH))
  
  val year = currentDate.format(DateTimeFormatter.ofPattern("yyyy", Locale.ENGLISH))

  val monthHeaders = "January" :: "February" :: "March" :: "April" :: "May" :: "June" :: "July" :: "August" :: "September" :: "October" :: "November" :: "December" :: Nil

  val spacerRow = List(List(","))

  def shipmentAmountPaid(shipment: Shipment) = {
    val amountPaid = tryo(shipment.amountPaid.get.toDouble).getOrElse(0D)
    val taxesPaid = tryo(shipment.taxPaid.get.toDouble).getOrElse(0D) 
    amountPaid - taxesPaid
  }

  def findProcessDateOfShipment(shipment: Shipment) = {
    shipment.dateProcessed.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()
  }

  def findMailedDateOfShipment(shipment: Shipment) = {
    tryo(shipment.dateShipped.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate())
  }

  def findCreatedDateOfUser(user: User) = {
    user.createdAt.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()
  }

  def findCancelledDateOfSubscription(subscription: Subscription) = {
    subscription.cancellationDate.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()
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

  def totalSalesForShipments(shipments: List[Shipment]) = {
    shipments.map { shipment =>
        shipmentAmountPaid(shipment)
    }.foldLeft(0D)(_+_)
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
        val processDate = findProcessDateOfShipment(shipment)

        val amountPaid = shipmentAmountPaid(shipment)
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
          if findCancelledDateOfSubscription(subscription).getYear == currentDate.getYear
      } yield {
        subscription
      }
    }

    val currentYearCancelShipments = currentYearSubscriptionCancels.map(_.shipments.toList).flatten.filter { shipment => 
      !findMailedDateOfShipment(shipment).isEmpty
    }

    val averageShipmentsPerCancelByYear = currentYearCancelShipments.size.toDouble/currentYearSubscriptionCancels.size.toDouble

    val currentYearCancelTotal = totalSalesForShipments(currentYearCancelShipments)

    val yearCancelCommisionAmount = currentYearCancelTotal * .35

    val currentYearCancelSalesRow = List(f"Year To Date Totals,${currentYearSubscriptionCancels.size},$averageShipmentsPerCancelByYear%3.2f,$$$currentYearCancelTotal,$$$yearCancelCommisionAmount%3.2f")

    val currentMonthSubscriptionCancels = currentYearSubscriptionCancels.filter { subscription =>
      val cancelDate = findCancelledDateOfSubscription(subscription)
      cancelDate.getMonth == currentDate.getMonth
    }

    val currentMonthCancelShipments = currentMonthSubscriptionCancels.map(_.shipments.toList).flatten.filter(shipment => !findMailedDateOfShipment(shipment).isEmpty)

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
          if !findMailedDateOfShipment(firstShipment).isEmpty
        lastShipment <- shipments.lastOption
      } yield {
        val firstShipmentDate = findProcessDateOfShipment(firstShipment)
        val lastShipmentDate = findProcessDateOfShipment(lastShipment)
        val shipmentCount = shipments.size
        val totalGrossSales = shipments.map(shipmentAmountPaid(_)).foldLeft(0D)(_+_)
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
      val processDate = findProcessDateOfShipment(shipment)

      processDate.getYear == currentDate.getYear
    }

    val currentYearTotal = totalSalesForShipments(currentYearShipments)

    val currentYearSalesRow = s"${year},$$${currentYearTotal}"

    val shipmentsByMonth = allShipments.groupBy { shipment =>
      val processDate = findProcessDateOfShipment(shipment)

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
    val processDate = findProcessDateOfShipment(shipment)

    val amountPaid = shipmentAmountPaid(shipment)
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
      val processDate = findProcessDateOfShipment(shipment)

      processDate.getYear == currentDate.getYear
    }

    val currentYearTotal = totalSalesForShipments(currentYearShipments)

    val yearCommisionAmount = currentYearTotal * .35

    val currentYearMTDSalesRow = List(f"Year To Date Totals,${totalUsers.size}")

    val usersByCurrentMonth = totalUsers.filter { user =>
      (findCreatedDateOfUser(user).getMonth == currentDate.getMonth) && (findCreatedDateOfUser(user).getYear == currentDate.getYear)
    }

    val shipmentsByCurrentMonth = allShipments.filter { shipment =>
      val mailedDate = findMailedDateOfShipment(shipment)

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
        
        val shipmentTotal = shipmentAmountPaid(shipment)
        val commisionTotal = shipmentTotal * .35

        f"$agentId,$customerId,${findProcessDateOfShipment(shipment)}"
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
}
