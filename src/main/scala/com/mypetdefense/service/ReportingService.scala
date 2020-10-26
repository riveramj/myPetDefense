package com.mypetdefense.service

import java.time._

import com.mypetdefense.model._
import com.mypetdefense.model.domain.reports
import com.mypetdefense.model.domain.reports._
import com.mypetdefense.snippet.admin.AmazonOrderExport
import com.mypetdefense.util.CSVHelper
import com.mypetdefense.util.CSVHelper.spacerRow
import com.mypetdefense.util.CalculationHelper._
import com.mypetdefense.util.DateHelper._
import com.mypetdefense.util.ModelSyntax._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._

object ReportingService extends Loggable {

  val myPetDefenseName = "My Pet Defense"
  val petlandName      = "Petland"

  def getUsersForAgency(agencyName: String): List[User] = {
    val agency = Agency.find(By(Agency.name, agencyName))
    agency.map(_.customers.toList).openOr(Nil)
  }

  def getFirstShipments(subscriptions: List[Subscription]): List[Shipment] = {
    val firstShipments = subscriptions.flatMap(_.shipments.headOption)

    firstShipments filter { shipment => !shipment.getMailedDateOfShipment.isEmpty }
  }

  def filterMailedShipments(shipments: List[Shipment]): List[Shipment] = {
    shipments filter { shipment =>
      val dateProcessed = shipment.getProcessDateOfShipment

      val legacyShipment_? = dateProcessed.isBefore(LocalDate.parse("2018-01-01"))

      !shipment.getMailedDateOfShipment.isEmpty || legacyShipment_?
    }
  }

  def getShipments(subscriptions: List[Subscription]): List[Shipment] = {
    subscriptions.flatMap(getShipments)
  }

  def getShipments(subscription: Subscription): List[Shipment] = {
    val shipments = subscription.shipments.toList

    filterMailedShipments(shipments)
  }

  def getPetCount(shipments: List[Shipment]): Int = {
    val lineItems = shipments.flatMap(_.shipmentLineItems)

    (for {
      lineItem <- lineItems
      ft       <- lineItem.fleaTick.obj
    } yield {
      ft
    }).size
  }

  def totalSalesForShipments(shipments: List[Shipment]): BigDecimal = {
    shipments.map { shipment => getShipmentAmountPaid(shipment) }.foldLeft(BigDecimal(0d))(_ + _)
  }

  def totalCommissionForSales(sales: BigDecimal): BigDecimal = sales * 0.35

  def findCurrentYearShipments(shipments: List[Shipment]): List[Shipment] = {
    shipments.filter { shipment =>
      val mailedDate = shipment.getMailedDateOfShipment

      mailedDate map { date =>
        date.getYear == currentDate.getYear
      } openOr false
    }
  }

  def findCurrentMonthShipments(shipments: List[Shipment], month: String = ""): List[Shipment] = {
    val date = getDateRange(month)

    shipments.filter { shipment =>
      val mailedDate = shipment.getMailedDateOfShipment

      mailedDate map { mailDate =>
        (mailDate.getYear == currentYear) &&
        (mailDate.getMonth == date.getMonth)
      } openOr false
    }
  }

  def findCurrentMonthProcessedShipments(
      shipments: List[Shipment],
      month: String = "",
      year: Int = 2019
  ): List[Shipment] = {
    val date = getDateRange(month)

    shipments.filter { shipment =>
      val processDate = shipment.getProcessDateOfShipment

      (processDate.getYear == year) && (processDate.getMonth == date.getMonth)
    }
  }

  def findCurrentMonthSubscriptions(subscriptions: List[Subscription]): List[Subscription] = {
    subscriptions.filter { subscription =>
      subscription.getCreatedDateOfSubscription.getMonth == currentDate.getMonth
    }
  }

  def findActiveSubscriptionsFirstMonth(subscriptions: List[Subscription]): List[Subscription] = {
    subscriptions.filter { subscription =>
      val createdDate      = subscription.getCreatedDateOfSubscription
      val cancellationDate = subscription.getCancelledDateOfSubscription
      if (cancellationDate.isEmpty) {
        true
      } else {
        val createdDateMonth = createdDate.getMonth
        val createdDateYear  = createdDate.getYear

        val cancelDateMonth = cancellationDate.map(_.getMonth)
        val cancelDateYear  = cancellationDate.map(_.getYear)

        cancelDateYear.map(_ == createdDateYear).openOr(false) &&
        cancelDateMonth.map(_ == createdDateMonth).openOr(false)
      }
    }
  }

  def findActiveSubscriptions(subscriptions: List[Subscription]): List[Subscription] = {
    subscriptions.filter { subscription => subscription.status.get != Status.Cancelled }
  }

  def findCustomersForAgent(customers: List[User], agentId: String): List[User] = {
    customers.filter(_.salesAgentId.get == agentId)
  }

  def findNewCustomersMonth(users: List[User], month: String = "", year: Int = 2019): List[User] = {
    val date = getDateRange(month)

    users filter { user =>
      (user.getCreatedDateOfUser.getYear == year) && (user.getCreatedDateOfUser.getMonth == date.getMonth)
    }
  }

  def findPayingUsers(users: List[User], month: String = "", year: Int = 2019): List[User] = {
    val date = getDateRange(month)

    users filter { user =>
      val userCreatedDate = user.getCreatedDateOfUser

      !(
        (userCreatedDate.getYear == year) &&
          (userCreatedDate.getMonth == date.getMonth)
      )
    }
  }

  def findPaidShipments(subscriptions: List[Subscription]): List[Shipment] = {
    val shipments = getShipments(subscriptions)

    shipments filter { shipment => getShipmentAmountPaid(shipment) > 0.0 }
  }

  def findCancelledSubscriptions(subscriptions: List[Subscription]): List[Subscription] = {
    subscriptions.filter(_.status.get == Status.Cancelled)
  }

  def findCurrentYearPayingCancelledSubscriptions(
      subscriptions: List[Subscription]
  ): List[Subscription] = {
    subscriptions.filter { subscription =>
      val createdDate      = subscription.getCreatedDateOfSubscription
      val cancellationDate = subscription.getCancelledDateOfSubscription
      if (cancellationDate.isEmpty) {
        false
      } else {
        val createdDateMonth = createdDate.getMonth

        val cancelDateMonth = cancellationDate.map(_.getMonth)
        val cancelDateYear  = cancellationDate.map(_.getYear)

        cancelDateYear.map(_ == currentDate.getYear).openOr(false) &&
        cancelDateMonth.map(_ != createdDateMonth).openOr(false)
      }
    }
  }

  def findCurrentMonthCancelledSubscriptions(
      subscriptions: List[Subscription],
      month: String = "",
      year: Int = 2019
  ): List[Subscription] = {
    val date = getDateRange(month, year)

    subscriptions filter { subscription =>
      val cancelDate = subscription.getCancelledDateOfSubscription

      cancelDate.map { cancelDate =>
        (cancelDate.getYear == year) &&
        (cancelDate.getMonth == date.getMonth)
      }.openOr(default = false)
    }
  }

  def exportRawSales(name: String): Box[LiftResponse] = {
    val data     = rawSalesReport(name)
    val fileName = s"salesData-$fileNameYearMonth.csv"
    Some(CSVHelper.inMemoryCsv(fileName, data))
  }

  def exportCancellationData(name: String): Some[InMemoryResponse] = {
    val possibleAgency     = Agency.find(By(Agency.name, name))
    val customers          = possibleAgency.map(_.customers.toList).openOr(Nil)
    val cancelledCustomers = customers.filter(_.status.get == Status.Cancelled)

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
        customer     <- cancelledCustomers
        subscription <- customer.subscription
        if subscription.getCancelledDateOfSubscription
          .map(_.getYear == currentDate.getYear)
          .openOr(false)
      } yield {
        subscription
      }
    }

    val currentYearCancelShipments =
      currentYearSubscriptionCancels.flatMap(_.shipments.toList).filter { shipment =>
        !shipment.getMailedDateOfShipment.isEmpty
      }

    val averageShipmentsPerCancelByYear =
      currentYearCancelShipments.size.toDouble / currentYearSubscriptionCancels.size.toDouble

    val currentYearCancelTotal = totalSalesForShipments(currentYearCancelShipments)

    val yearCancelCommisionAmount = currentYearCancelTotal * .35

    val currentYearCancelSalesRow = List(
      f"Year To Date Totals,${currentYearSubscriptionCancels.size},$averageShipmentsPerCancelByYear%3.2f,$$$currentYearCancelTotal,$$$yearCancelCommisionAmount%3.2f"
    )

    val currentMonthSubscriptionCancels = currentYearSubscriptionCancels.filter { subscription =>
      val cancelDate = subscription.getCancelledDateOfSubscription
      cancelDate.map(_.getMonth == currentDate.getMonth).openOr(false)
    }

    val currentMonthCancelShipments = currentMonthSubscriptionCancels
      .flatMap(_.shipments.toList)
      .filter(shipment => !shipment.getMailedDateOfShipment.isEmpty)

    val averageShipmentsPerCancelByMonth =
      currentMonthCancelShipments.size.toDouble / currentMonthSubscriptionCancels.size.toDouble

    val currentMonthCancelTotal = totalSalesForShipments(currentMonthCancelShipments)

    val monthCancelCommisionAmount = currentMonthCancelTotal * .35

    val currentMonthCancelSalesRow = List(
      f"Month To Date Totals,${currentMonthSubscriptionCancels.size},$averageShipmentsPerCancelByMonth%3.2f,$$$currentMonthCancelTotal,$$$monthCancelCommisionAmount%3.2f"
    )

    val headers =
      "Customer Id" :: "Start Month" :: "End Month" :: "Customer Status" :: "Shipment Count" :: "Total Gross Sales" :: "Total Commission" :: Nil

    val allCancellationRows: List[List[String]] = {
      for {
        customer     <- customers.filter(_.status.get != Status.Active)
        subscription <- customer.subscription
        shipments = subscription.shipments.toList.sortBy(_.dateProcessed.get.getTime)
        firstShipment <- shipments.headOption
        if !firstShipment.getMailedDateOfShipment.isEmpty
        lastShipment <- shipments.lastOption
      } yield {
        val firstShipmentDate = firstShipment.getProcessDateOfShipment
        val lastShipmentDate  = lastShipment.getProcessDateOfShipment
        val shipmentCount     = shipments.size
        val totalGrossSales   = shipments.map(getShipmentAmountPaid).foldLeft(BigDecimal(0d))(_ + _)
        val totalCommission   = totalGrossSales * .35

        customer.userId.toString ::
          firstShipmentDate.getMonth.toString ::
          lastShipmentDate.getMonth.toString ::
          subscription.status.toString ::
          shipmentCount.toString ::
          s"$$$totalGrossSales" ::
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

    val fileName = s"cancellations-$fileNameYearMonth.csv"

    Some(CSVHelper.generateCSV(csvRows, fileName))
  }

  def exportTotalSales(name: String): Box[LiftResponse] = {
    val allShipments = {
      for {
        agency       <- Agency.find(By(Agency.name, name)).toList
        customer     <- agency.customers.toList
        subscription <- customer.subscription.toList
        shipment     <- subscription.shipments.toList.sortBy(_.dateProcessed.get.getTime)
      } yield {
        shipment
      }
    }

    val currentYearShipments = allShipments.filter { shipment =>
      val processDate = shipment.getProcessDateOfShipment

      processDate.getYear == currentDate.getYear
    }

    val currentYearTotal = totalSalesForShipments(currentYearShipments)

    val currentYearSalesRow = s"$thisYear,$$$currentYearTotal"

    val shipmentsByMonth = allShipments.groupBy { shipment =>
      val processDate = shipment.getProcessDateOfShipment

      processDate.getMonth
    }

    val totalSalesByMonth = shipmentsByMonth.map {
      case (month, shipments) =>
        (month, s"$$${totalSalesForShipments(shipments)}")
    }

    val salesMonthsHeaders = totalSalesByMonth.keys.mkString(",")
    val salesMonthsTotals  = totalSalesByMonth.values.mkString(",")

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
    val data = exportMonthToDateSalesReport(name)

    val fileName = s"month-to-date-salesData-$fileNameMonthDayYear.csv"

    Some(CSVHelper.inMemoryCsv(fileName, data))
  }

  def exportAmazonOrders(amazonOrderExport: AmazonOrderExport): Box[LiftResponse] = {
    println(amazonOrderExport + " 000000")

    val dateFormat      = AmazonOrderReport.dateFormat
    val startDateExport = amazonOrderExport.startDate.map(dateFormat.parse)
    val endDateExport   = amazonOrderExport.endDate.map(dateFormat.parse)
    val petExport       = amazonOrderExport.animalType

    println(petExport)

    val data: List[AmazonOrderReport] =
      for {
        startDate <- startDateExport.toList
        endDate   <- endDateExport.toList
        pet       <- petExport.toList
        order     <- AmazonOrder.findOrdersToReport(startDate, endDate, pet)
      } yield order

    val fileName = s"amazon-orders-$startDateExport-$endDateExport.csv"

    Some(CSVHelper.inMemoryCsv(fileName, data))
  }

  def exportAgencyMtdYtdSales(name: String): Box[LiftResponse] = {
    val data     = agencyMtdYtdSalesReport(name)
    val fileName = s"$name-mtd-ytd-$fileNameMonthDayYear.csv"

    Some(CSVHelper.inMemoryCsv(fileName, data))
  }

  def findYesterdayNewSales: List[User] = {
    User.findAll(
      By_>=(User.createdAt, yesterdayStart),
      By_<(User.createdAt, yesterdayEnd)
    )
  }

  def yesterdayShipments: (Int, Int, BigDecimal) = {
    val yesterdayShipments = Shipment.findAll(
      By_>=(Shipment.dateShipped, yesterdayStart),
      By_<(Shipment.dateShipped, yesterdayEnd)
    )

    val paidShipments  = yesterdayShipments.filter(_.amountPaid.get != "0")
    val newShipments   = yesterdayShipments diff paidShipments
    val paidGrossSales = totalSalesForShipments(paidShipments)

    (newShipments.size, paidShipments.size, paidGrossSales)
  }

  def yesterdayCancels: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.cancellationDate, yesterdayStart),
      By_<(Subscription.cancellationDate, yesterdayEnd)
    )
  }

  def findYesterdaySalesByAgency: List[(String, Int)] = {
    val agencies = Agency.findAll(
      NotBy(Agency.name, myPetDefenseName),
      NotBy(Agency.name, petlandName)
    )

    val yesterdayCreatedUsersByAgencies = agencies.map { agency =>
      (
        agency,
        User.findAll(
          By(User.referer, agency),
          By_>=(User.createdAt, yesterdayStart),
          By_<(User.createdAt, yesterdayEnd)
        )
      )
    }

    yesterdayCreatedUsersByAgencies.map {
      case (agency, users) =>
        val pets = users.flatMap(_.pets.toList)
        agency.name.get -> pets.size
    }.sortBy(_._1)
  }

  def findMtdShipments: List[Shipment] = {
    Shipment.findAll(
      By_>=(Shipment.createdAt, monthDayOne)
    )
  }

  def findTodayShipments: List[Shipment] = {
    Shipment.findAll(
      By_>=(Shipment.dateProcessed, nowDate)
    )
  }

  def findCurrentMonthUpcomingSubscriptions: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.nextShipDate, tomorrowStart),
      By_<(Subscription.nextShipDate, beginngNextMonth)
    )
  }

  def findNewTodaySubscriptions: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, nowDate)
    )
  }

  def findNewTodaySubscriptionsLastMonth: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, todayLastMonth),
      By_<(Subscription.createdAt, todayLastMonthEnd)
    )
  }

  def findNewTodaySubscriptionsLastYear: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, todayLastYear),
      By_<(Subscription.createdAt, todayLastYearEnd)
    )
  }

  def findNewMTDSubscriptions: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, monthDayOne)
    )
  }

  def findNewMTDSubscriptionsLastMonth: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, monthDayOneLastMonth),
      By_<(Subscription.createdAt, currentDayLastMonthEnd)
    )
  }

  def findNewMTDSubscriptionsLastYear: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, monthDayOneLastYear),
      By_<(Subscription.createdAt, currentDayLastYearEnd)
    )
  }

  def findNewYTDSubscriptions: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, yearDayOne)
    )
  }

  def findNewYTDSubscriptionsLastMonth: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, yearDayOne),
      By_<(Subscription.createdAt, todayLastMonthEnd)
    )
  }

  def findNewYTDSubscriptionsLastYear: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.createdAt, yearDayOneLastYear),
      By_<(Subscription.createdAt, currentDayLastYearEnd)
    )
  }

  def findCancelledMtdSubscriptions: List[Subscription] = {
    Subscription.findAll(
      By_>=(Subscription.cancellationDate, monthDayOne)
    )
  }

  def findMTDSalesByAgency: List[(String, Int)] = {
    val agencies = Agency.findAll(
      NotBy(Agency.name, myPetDefenseName),
      NotBy(Agency.name, petlandName)
    )

    val newUsersThisMonthByAgency = agencies.map { agency =>
      (
        agency,
        User.findAll(
          By(User.referer, agency),
          By_>=(User.createdAt, monthDayOne),
          By_<(User.createdAt, beginngNextMonth)
        )
      )
    }

    newUsersThisMonthByAgency.map {
      case (agency, users) =>
        val pets = users.flatMap(_.pets)
        agency.name.get -> pets.size
    }.sortBy(_._1)
  }

  def findYesterdaySalesByAgent: List[(String, Int)] = {
    val newUsersYesterday = Agency
      .findAll(
        NotBy(Agency.name, myPetDefenseName),
        NotBy(Agency.name, petlandName)
      )
      .flatMap { agency =>
        User.findAll(
          By(User.referer, agency),
          By_>=(User.createdAt, yesterdayStart),
          By_<(User.createdAt, yesterdayEnd)
        )
      }

    newUsersYesterday
      .groupBy(_.salesAgentId.get)
      .map {
        case (agentId, users) =>
          val pets = users.flatMap(_.pets)
          agentId -> pets.size
      }
      .toList
      .sortBy(_._1)
  }

  def findMTDSalesByAgent: List[(String, Int)] = {
    val newUsersThisMonth = Agency
      .findAll(
        NotBy(Agency.name, myPetDefenseName),
        NotBy(Agency.name, petlandName)
      )
      .flatMap { agency =>
        User.findAll(
          By(User.referer, agency),
          By_>=(User.createdAt, monthDayOne),
          By_<(User.createdAt, beginngNextMonth)
        )
      }

    newUsersThisMonth
      .groupBy(_.salesAgentId.get)
      .map {
        case (agentId, users) =>
          val pets = users.flatMap(_.pets)
          agentId -> pets.size
      }
      .toList
      .sortBy(_._1)
  }

  def exportAgencyMonthSales(
      name: String,
      month: String,
      possibleYear: String
  ): Box[LiftResponse] = {
    val year = getYearOrCurrent(possibleYear)
    val data = agencyMonthSales(name, month, year)

    val fileName = s"$name-$month-$year-sales-summary.csv"

    Some(CSVHelper.inMemoryCsv(fileName, data))
  }

  def exportSameDayCancels(name: String): Box[LiftResponse] = {
    val data     = sameDayCancelsReport(name)
    val fileName = s"sameDayCancels-${LocalDate.now()}.csv"
    Some(CSVHelper.inMemoryCsv(fileName, data))
  }

  def cancelsByShipment(subscriptions: List[Subscription]): List[(String, Int)] = {
    def findCancellationShipmentSize(subscriptions: List[Subscription]) = {
      subscriptions.map { subscription =>
        val shipments = subscription.shipments.toList

        val mailedShipments = filterMailedShipments(shipments)

        mailedShipments.size
      }
    }

    val cancellations = subscriptions.filter(_.status.get == Status.Cancelled)

    val cancellationShipments = findCancellationShipmentSize(cancellations)

    val cancellationTimes = List(0, 1, 2, 3, 4, 5).map { count =>
      val totalForCount = cancellationShipments.count(_ == count)

      (count.toString, totalForCount)
    } ++ List(("6+", cancellationShipments.count(_ >= 6)))

    cancellationTimes
  }

  def sameDayCancelsByMonth(subscriptions: List[Subscription]): Map[Month, Int] = {
    def findSameDayCancellations(subscriptions: List[Subscription]) = {
      subscriptions.filter { subscription =>
        val shipments = subscription.shipments.toList

        val mailedShipments = filterMailedShipments(shipments)

        mailedShipments.isEmpty
      }
    }

    val cancellations = subscriptions.filter(_.status.get == Status.Cancelled)

    val sameDayCancels = findSameDayCancellations(cancellations)

    val cancelsByMonth = sameDayCancels.groupBy { subscription =>
      subscription.getCreatedDateOfSubscription.getMonth
    }

    cancelsByMonth.map {
      case (month, subscriptions) =>
        (month, subscriptions.size)
    }
  }

  def exportAgencyCustomers(rawAgencyId: String): Box[LiftResponse] =
    for {
      agencyId <- tryo(rawAgencyId.toLong)
      data     <- agencyNameAndCustomerReport(agencyId)
      fileName = s"${data.agencyName}-customers-$fileNameMonthDayYear.csv"
      csv <- Some(CSVHelper.inMemoryCsv(fileName, data))
    } yield csv

  def agencyNameAndCustomerReport(agencyId: Long): Box[AgencyCustomersReport] = {
    for {
      agency <- Agency.find(By(Agency.agencyId, agencyId))
      agencyName    = agency.name.get
      customersInfo = customersReport(agency)
    } yield reports.AgencyCustomersReport(agencyName, customersInfo)
  }

  def customersReport(agency: Agency): List[CustomerDataReport] =
    for {
      customer     <- agency.customers.toList
      subscription <- customer.subscription.toList
    } yield {
      val startDate  = subscription.getStartDateOfSubscription
      val cancelDate = subscription.getCancelDateOfSubscription
      reports.CustomerDataReport(
        customer.name,
        subscription.status.get,
        subscription.shipments.toList.size,
        startDate,
        cancelDate
      )
    }

  private[service] def rawSalesReport(agencyName: String): List[RawSaleDataReport] = {
    for {
      agency       <- Agency.find(By(Agency.name, agencyName)).toList
      customer     <- agency.customers.toList
      subscription <- customer.subscription.toList
      shipment     <- subscription.shipments.toList.sortBy(_.dateProcessed.get.getTime)
      mailedDate   <- shipment.getMailedDateOfShipment
    } yield {
      val amountPaid = getShipmentAmountPaid(shipment)
      val commision  = amountPaid * .35

      RawSaleDataReport(
        mailedDate.getYear,
        mailedDate.getMonth,
        mailedDate,
        customer.userId.get,
        customer.name,
        amountPaid,
        customer.salesAgentId.get,
        commision,
        subscription.status.get
      )
    }
  }

  private[service] def sameDayCancelsReport(agencyName: String): List[CancelsInMonthReport] = {
    for {
      agency <- Agency.find(By(Agency.name, agencyName)).toList
      customers     = agency.customers.toList
      subscriptions = customers.flatMap(_.subscription.obj)
      cancelsByMonth <- User.sameDayCancelsByMonth(subscriptions)
    } yield CancelsInMonthReport(cancelsByMonth._1, cancelsByMonth._2)
  }

  private[service] def agencyMonthSales(
      name: String,
      month: String,
      year: Int
  ): AgencyMonthSalesReport = {
    val totalUsers = getTotalUsers(name)

    val totalSubscriptions = totalUsers.getSubscriptions
    val totalCancelledSubscriptions =
      findCurrentMonthCancelledSubscriptions(totalSubscriptions, month, year)

    val newUsersMonth    = findNewCustomersMonth(totalUsers, month, year)
    val netNewUsersMonth = newUsersMonth.filter(_.status.get != Status.Cancelled)
    val payingUsers      = findPayingUsers(totalUsers, month, year)

    val allPayingSubscriptions = payingUsers.getSubscriptions

    val allPaidShipments          = findPaidShipments(allPayingSubscriptions)
    val paidMonthShipments        = findCurrentMonthProcessedShipments(allPaidShipments, month, year)
    val paidMonthPetsShippedCount = getPetCount(paidMonthShipments)
    val paidMonthGrossSales       = totalSalesForShipments(paidMonthShipments)
    val paidMonthCommission       = totalCommissionForSales(paidMonthGrossSales)
    val discountCount             = (paidMonthPetsShippedCount * BigDecimal(12.99)) - paidMonthGrossSales

    AgencyMonthSalesReport(
      year,
      month,
      netNewUsersMonth.size,
      totalCancelledSubscriptions.size,
      paidMonthShipments.size,
      paidMonthPetsShippedCount,
      discountCount,
      paidMonthGrossSales,
      paidMonthCommission
    )
  }

  private def genAllSalesForMonthReport(
      input: Map[String, Iterable[Shipment]]
  ): String =
    input.map {
      case (agentId, shipments) =>
        shipments.map { shipment =>
          val customerId = {
            for {
              subscription <- shipment.subscription.obj
              user         <- subscription.user.obj
            } yield {
              user.userId.toString
            }
          }.openOr("-")

          f"$agentId,$customerId,${shipment.getProcessDateOfShipment}"
        }.mkString("\n")
    }.mkString("\n")

  private[service] def exportMonthToDateSalesReport(agencyName: String): AgencyMtdSalesReport = {
    val agency                  = Agency.find(By(Agency.name, agencyName)).headOption
    val totalUsers              = agency.map(_.customers.toList).getOrElse(Nil)
    val shipmentsByCurrentMonth = agency.toList.flatMap(_.currentMonthShipments)
    val usersByCurrentMonth = totalUsers.filter { user =>
      (user.getCreatedDateOfUser.getMonth == currentDate.getMonth) && (user.getCreatedDateOfUser.getYear == currentDate.getYear)
    }
    val usersMonthByAgent = usersByCurrentMonth.groupBy { user => user.salesAgentId.get }
    val agentSalesData: List[UsersMonthByAgentReport] = usersMonthByAgent.map {
      case (agentId, users) =>
        UsersMonthByAgentReport(agentId, users.size)
    }.toList

    val shipmentMonthByAgent: Map[String, Iterable[Shipment]] = shipmentsByCurrentMonth.groupBy {
      shipment =>
        val user = {
          for {
            subscription <- shipment.subscription.obj
            user         <- subscription.user.obj
          } yield {
            user
          }
        }
        user.map(_.salesAgentId.get).openOr("")
    }
    val allSalesForMonthData = genAllSalesForMonthReport(shipmentMonthByAgent)
    AgencyMtdSalesReport(
      totalUsers.size,
      usersByCurrentMonth.size,
      agentSalesData,
      allSalesForMonthData
    )
  }

  private[service] def agencyMtdYtdSalesReport(agencyName: String): AgencyMtdYtdSalesReport = {
    val totalUsers                = Agency.find(By(Agency.name, agencyName)).map(_.customers.toList).getOrElse(Nil)
    val newUsersYear              = totalUsers.filter(_.getCreatedDateOfUser.getYear == currentDate.getYear)
    val newUsersYearSubscriptions = newUsersYear.getSubscriptions
    val newUsersYearSubscriptionFirstMonthActive = findActiveSubscriptionsFirstMonth(
      newUsersYearSubscriptions
    )
    val newUsersYearSubscriptionCancelled =
      newUsersYearSubscriptions diff newUsersYearSubscriptionFirstMonthActive
    val newUsersMonthSubscriptionCancelled = findCurrentMonthSubscriptions(
      newUsersYearSubscriptionCancelled
    )
    val newUsersMonthSubscriptionActive = findCurrentMonthSubscriptions(
      newUsersYearSubscriptionFirstMonthActive
    )
    val newUsersYearShipments  = getFirstShipments(newUsersYearSubscriptions)
    val newUsersMonthShipments = findCurrentMonthShipments(newUsersYearShipments)
    val allPayingCustomers     = findPayingUsers(totalUsers)

    val allPayingSubscriptions = allPayingCustomers.getSubscriptions
    val allPayingActiveSubscriptions: List[Subscription] = findActiveSubscriptions(
      allPayingSubscriptions
    )
    val allPayingCancelledSubscriptions = findCancelledSubscriptions(allPayingSubscriptions)
    val paidSubscriptionMonthCancelled: List[Subscription] = findCurrentMonthCancelledSubscriptions(
      allPayingCancelledSubscriptions
    )
    val allPaidShipments   = findPaidShipments(allPayingSubscriptions)
    val paidMonthShipments = findCurrentMonthShipments(allPaidShipments)

    val mtdNewCustomers = getMtdNewCustomers(
      newUsersYear,
      newUsersMonthSubscriptionCancelled,
      newUsersMonthSubscriptionActive,
      newUsersMonthShipments
    )

    val ytdNewCustomers = getYtdNewCustomers(
      newUsersYearSubscriptions,
      newUsersYearSubscriptionCancelled,
      newUsersYearSubscriptionFirstMonthActive,
      newUsersYearShipments
    )
    val mtdPayingCustomers = getMtdPayingCustomers(
      allPayingActiveSubscriptions,
      paidSubscriptionMonthCancelled,
      paidMonthShipments
    )

    val ytdPayingCustomers = getYtdPayingCustomers(
      allPayingActiveSubscriptions,
      allPayingCancelledSubscriptions,
      allPaidShipments
    )
    val paidCustomersByAgent = getPaidCustomersByAgent(
      paidMonthShipments,
      allPayingActiveSubscriptions,
      paidSubscriptionMonthCancelled
    )

    val newCustomersByAgent = getNewCustomersByAgent(
      newUsersMonthShipments,
      newUsersMonthSubscriptionActive,
      newUsersMonthSubscriptionCancelled
    )
    AgencyMtdYtdSalesReport(
      mtdNewCustomers,
      ytdNewCustomers,
      mtdPayingCustomers,
      ytdPayingCustomers,
      paidCustomersByAgent,
      newCustomersByAgent
    )
  }

  private def getNewCustomersByAgent(
      newUsersMonthShipments: List[Shipment],
      newUsersMonthSubscriptionActive: List[Subscription],
      newUsersMonthSubscriptionCancelled: List[Subscription]
  ): List[NewCustomersByAgent] =
    newUsersMonthShipments.groupBy { shipment =>
      val user = {
        for {
          subscription <- shipment.subscription.obj
          user         <- subscription.user.obj
        } yield {
          user
        }
      }

      user.map(_.salesAgentId.get).openOr("")
    }.map {
      case (agentId, shipments) =>
        val shipmentsPetCount = getPetCount(shipments)

        val activeNewUsers = newUsersMonthSubscriptionActive.flatMap(_.user.obj.toList)

        val cancelledNewUsers = newUsersMonthSubscriptionCancelled.flatMap(_.user.obj.toList)

        val activeNewAgentCustomer    = findCustomersForAgent(activeNewUsers, agentId)
        val cancelledNewAgentCustomer = findCustomersForAgent(cancelledNewUsers, agentId)
        NewCustomersByAgent(
          agentId,
          activeNewAgentCustomer.size,
          cancelledNewAgentCustomer.size,
          shipments.size,
          shipmentsPetCount
        )
    }.toList

  private def getPaidCustomersByAgent(
      paidMonthShipments: List[Shipment],
      allPayingActiveSubscriptions: List[Subscription],
      paidSubscriptionMonthCancelled: List[Subscription]
  ): List[PaidCustomersByAgentMtdYtdSalesReport] =
    paidMonthShipments.groupBy { shipment =>
      val user = {
        for {
          subscription <- shipment.subscription.obj
          user         <- subscription.user.obj
        } yield {
          user
        }
      }
      user.map(_.salesAgentId.get).openOr("")
    }.map {
      case (agentId, shipments) =>
        val shipmentsPetCount = getPetCount(shipments)
        val shipmentsTotal    = totalSalesForShipments(shipments)
        val commisionTotal    = totalCommissionForSales(shipmentsTotal)

        val activePayingUsers = allPayingActiveSubscriptions.flatMap(_.user.obj.toList)

        val cancelledMonthPaidUsers = paidSubscriptionMonthCancelled.flatMap(_.user.obj.toList)

        val activePayingAgentCustomer    = findCustomersForAgent(activePayingUsers, agentId)
        val cancelledPayingAgentCustomer = findCustomersForAgent(cancelledMonthPaidUsers, agentId)
        PaidCustomersByAgentMtdYtdSalesReport(
          agentId,
          activePayingAgentCustomer.size,
          cancelledPayingAgentCustomer.size,
          shipments.size,
          shipmentsPetCount,
          shipmentsTotal,
          commisionTotal
        )
    }.toList

  private def getMtdPayingCustomers(
      allPayingActiveSubscriptions: List[Subscription],
      paidSubscriptionMonthCancelled: List[Subscription],
      paidMonthShipments: List[Shipment]
  ): PayingCustomersMtdYtdSalesReport = {
    val paidMonthPetsShippedCount = getPetCount(paidMonthShipments)
    val paidMonthGrossSales       = totalSalesForShipments(paidMonthShipments)
    val paidMonthCommission       = totalCommissionForSales(paidMonthGrossSales)
    PayingCustomersMtdYtdSalesReport(
      allPayingActiveSubscriptions.size,
      paidSubscriptionMonthCancelled.size,
      paidMonthShipments.size,
      paidMonthPetsShippedCount,
      paidMonthGrossSales,
      paidMonthCommission
    )
  }

  private def getYtdPayingCustomers(
      allPayingActiveSubscriptions: List[Subscription],
      allPayingCancelledSubscriptions: List[Subscription],
      allPaidShipments: List[Shipment]
  ): PayingCustomersMtdYtdSalesReport = {
    val paidYearShipments: List[Shipment] = findCurrentYearShipments(allPaidShipments)
    val paidYearPetsShippedCount          = getPetCount(paidYearShipments)
    val paidYearGrossSales                = totalSalesForShipments(paidYearShipments)
    val paidYearCommission                = totalCommissionForSales(paidYearGrossSales)
    val paidSubscriptionYearCancelled = findCurrentYearPayingCancelledSubscriptions(
      allPayingCancelledSubscriptions
    )
    PayingCustomersMtdYtdSalesReport(
      allPayingActiveSubscriptions.size,
      paidSubscriptionYearCancelled.size,
      paidYearShipments.size,
      paidYearPetsShippedCount,
      paidYearGrossSales,
      paidYearCommission
    )
  }

  private def getMtdNewCustomers(
      newUsersYear: List[User],
      newUsersMonthSubscriptionCancelled: List[Subscription],
      newUsersMonthSubscriptionActive: List[Subscription],
      newUsersMonthShipments: List[Shipment]
  ): NewCustomersMtdYtdSalesReport = {
    val newUsersMonth = newUsersYear.filter { user =>
      user.getCreatedDateOfUser.getMonth == currentDate.getMonth
    }
    val newUsersMonthShippedPetCount = getPetCount(newUsersMonthShipments)

    NewCustomersMtdYtdSalesReport(
      newUsersMonth.size,
      newUsersMonthSubscriptionCancelled.size,
      newUsersMonthSubscriptionActive.size,
      newUsersMonthShipments.size,
      newUsersMonthShippedPetCount
    )
  }

  private def getYtdNewCustomers(
      newUsersYearSubscriptions: List[Subscription],
      newUsersYearSubscriptionCancelled: List[Subscription],
      newUsersYearSubscriptionFirstMonthActive: List[Subscription],
      newUsersYearShipments: List[Shipment]
  ): NewCustomersMtdYtdSalesReport = {
    val newUsersYearShippedPetCount = getPetCount(newUsersYearShipments)
    NewCustomersMtdYtdSalesReport(
      newUsersYearSubscriptions.size,
      newUsersYearSubscriptionCancelled.size,
      newUsersYearSubscriptionFirstMonthActive.size,
      newUsersYearShipments.size,
      newUsersYearShippedPetCount
    )
  }

  private def getTotalUsers(agencyName: String) =
    if (agencyName != "TPP")
      Agency.find(By(Agency.name, agencyName)).map(_.customers.toList).getOrElse(Nil)
    else {
      val puppySpot =
        Agency.find(By(Agency.name, "PuppySpot")).map(_.customers.toList).getOrElse(Nil)
      val petland =
        Agency.find(By(Agency.name, petlandName)).map(Agency.getAllChildrenCustomers).getOrElse(Nil)

      puppySpot ++ petland
    }

}
