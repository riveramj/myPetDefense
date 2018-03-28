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
  
  val year = currentDate.format(DateTimeFormatter.ofPattern("yyyy", Locale.ENGLISH))

  val monthHeaders = "January" :: "February" :: "March" :: "April" :: "May" :: "June" :: "July" :: "August" :: "September" :: "October" :: "November" :: "December" :: Nil

  def shipmentAmountPaid(shipment: Shipment) = {
    val amountPaid = tryo(shipment.amountPaid.get.toDouble).getOrElse(0D)
    val taxesPaid = tryo(shipment.taxPaid.get.toDouble).getOrElse(0D) 
    amountPaid - taxesPaid
  }

  def findProcessDateOfShipment(shipment: Shipment) = {
    shipment.dateProcessed.get.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()
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

  def exportRawSales: Box[LiftResponse] = {
    val headers = "Year" :: "Month" :: "Date" :: "Customer Id" :: "Customer Name" :: "Amount" :: "Call Agent Id" :: "Commision" :: "Customer Status" :: Nil

    val csvRows: List[List[String]] = {
      for {
        agency <- Agency.find(By(Agency.name, "TPP")).toList
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

  def exportCancellationData = {
    val possibleAgency = Agency.find(By(Agency.name, "TPP"))
    val customers = possibleAgency.map(_.customers.toList).openOr(Nil)

    val customerStatusBreakdown = customers.groupBy { customer =>
      customer.subscription.map(_.status.get)
    }

    val customerStatusTotals = customerStatusBreakdown.map { customer =>
      s"${customer._1.headOption.getOrElse("")} Users,${customer._2.size.toString}"
    }.toList.sorted.map(_ :: "," :: Nil)

    val headers = "Customer Id" :: "Start Month" :: "End Month" :: "Customer Status" :: "Shipment Count" :: "Total Gross Sales" :: "Total Commission" :: Nil

    val csvRows: List[List[String]] = {
      for {
        agency <- possibleAgency.toList
        customer <- customers.filter(_.status != Status.Active)
        subscription <- customer.subscription
        shipments = subscription.shipments.toList.sortBy(_.dateProcessed.get.getTime)
        firstShipment <- shipments.headOption
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

    val spacerRow = List(List(","))

    val csv = (
      customerStatusTotals ++
      spacerRow ++
      List(headers) ++
      csvRows
    ).map(_.mkString(",")).mkString("\n")

    val fileName = s"cancellations-${fileNameYearMonth}.csv"

    val file = "filename=\"" + fileName + "\""

    generateCSV(csv, file)
  }

  def exportTotalSales: Box[LiftResponse] = {
    val allShipments = {
      for {
        agency <- Agency.find(By(Agency.name, "TPP")).toList
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

  def exportMonthToDateSales: Box[LiftResponse] = {
    val allShipments = {
      for {
        agency <- Agency.find(By(Agency.name, "TPP")).toList
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

    val yearCommisionAmount = currentYearTotal*.35

    val currentYearSalesRow = f"Year To Date Totals,${currentYearShipments.size},$$$currentYearTotal,$$$yearCommisionAmount%3.2f"

    val shipmentsByCurrentMonth = allShipments.filter { shipment =>
      val processDate = findProcessDateOfShipment(shipment)

      processDate.getMonth == currentDate.getMonth
    }

    val currentMonthTotal = totalSalesForShipments(shipmentsByCurrentMonth)

    val monthCommisionAmount = currentMonthTotal*.35

    val currentMonthSalesRow = f"Year To Date Totals,${shipmentsByCurrentMonth.size},$$$currentMonthTotal,$$$monthCommisionAmount%3.2f"

    println("===============")
    println(currentYearSalesRow)
    println("========= year ^")
    println("===============")
    println(currentMonthSalesRow)
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
}
