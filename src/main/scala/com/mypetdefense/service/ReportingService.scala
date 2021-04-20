package com.mypetdefense.service

import com.mypetdefense.model._
import com.mypetdefense.model.domain.reports
import com.mypetdefense.model.domain.reports._
import com.mypetdefense.util.csv.CSVHelper
import com.mypetdefense.util.CalculationHelper._
import com.mypetdefense.util.DateHelper._
import com.mypetdefense.util.ModelSyntax._
import com.mypetdefense.util.{CalculationHelper, DateHelper}
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import net.liftweb.util.Props

import java.time._
import java.util.Date
import scala.math.BigDecimal.double2bigDecimal

case class SnapshotStatistics(agency: Agency, boxStatistics: BoxStatistics)
case class BoxStatistics(boxType: BoxType.Value, boxCount: Int, subscriptionCount: Int)

object ReportingService extends Loggable {

  val tppName          = "TPP"
  val myPetDefenseName = "My Pet Defense"
  val petlandName      = "Petland"

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
    val date = getDateRange(month, year)

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
    CSVHelper.inMemoryCsv(fileName, data)
  }

  def exportCancellationData(name: String): Box[InMemoryResponse] = {
    val data     = exportCancellationReport(name)
    val fileName = s"cancellations-$fileNameYearMonth.csv"

    CSVHelper.inMemoryCsv(fileName, data)
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

    CSVHelper.inMemoryCsv(fileName, data)
  }

  def exportAgencyMtdYtdSales(name: String): Box[LiftResponse] = {
    val data     = agencyMtdYtdSalesReport(name)
    val fileName = s"$name-mtd-ytd-$fileNameMonthDayYear.csv"

    CSVHelper.inMemoryCsv(fileName, data)
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

  def findYesterdayMTDSalesByAgency: List[(String, Int)] = {
    val agencies = Agency.findAll(
      NotBy(Agency.name, myPetDefenseName),
      NotBy(Agency.name, petlandName)
    )

    val newUsersThisMonthByAgency = agencies.map { agency =>
      (
        agency,
        User.findAll(
          By(User.referer, agency),
          By_>=(User.createdAt, yesterdayMonthStart),
          By_<(User.createdAt, yesterdayMonthEnd)
        )
      )
    }

    newUsersThisMonthByAgency.map {
      case (agency, users) =>
        val pets = users.flatMap(_.pets)
        agency.name.get -> pets.size
    }.sortBy(_._1)
  }

  def findLastMonthSalesByAgency: List[(String, Int)] = {
    val agencies = Agency.findAll(
      NotBy(Agency.name, myPetDefenseName),
      NotBy(Agency.name, petlandName)
    )

    val newUsersLastMonthByAgency = agencies.map { agency =>
      (
        agency,
        User.findAll(
          By(User.referer, agency),
          By_>=(User.createdAt, monthDayOneLastMonth),
          By_<(User.createdAt, currentDayLastMonthEnd)
        )
      )
    }

    newUsersLastMonthByAgency.map {
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

  def findYesterdayMTDSalesByAgent: List[(String, Int)] = {
    val newUsersThisMonth = Agency
      .findAll(
        NotBy(Agency.name, myPetDefenseName),
        NotBy(Agency.name, petlandName)
      )
      .flatMap { agency =>
        User.findAll(
          By(User.referer, agency),
          By_>=(User.createdAt, yesterdayMonthStart),
          By_<(User.createdAt, yesterdayMonthEnd)
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

  def findLastMonthSalesByAgent: List[(String, Int)] = {
    val newUsersLastMonth = Agency
      .findAll(
        NotBy(Agency.name, myPetDefenseName),
        NotBy(Agency.name, petlandName)
      )
      .flatMap { agency =>
        User.findAll(
          By(User.referer, agency),
          By_>=(User.createdAt, monthDayOneLastMonth),
          By_<(User.createdAt, currentDayLastMonthEnd)
        )
      }

    newUsersLastMonth
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

    CSVHelper.inMemoryCsv(fileName, data)
  }

  def exportSameDayCancels(name: String): Box[LiftResponse] = {
    val data     = sameDayCancelsReport(name)
    val fileName = s"sameDayCancels-${LocalDate.now()}.csv"
    CSVHelper.inMemoryCsv(fileName, data)
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

  def exportAgencyCustomers(rawAgencyId: String): Box[LiftResponse] =
    for {
      agencyId <- tryo(rawAgencyId.toLong)
      data     <- agencyNameAndCustomerReport(agencyId)
      fileName = s"${data.agencyName}-customers-$fileNameMonthDayYear.csv"
      csv <- CSVHelper.inMemoryCsv(fileName, data)
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

  def executiveSnapshot: Box[InMemoryResponse] = {
    val data     = executiveSnapshotReport
    val fileName = s"executive-snapshot-${LocalDate.now()}.csv"
    CSVHelper.inMemoryCsv(fileName, data)
  }

  def executiveDashboardReport: ExecutiveDashboardReport = {
    val newStartsTodayData = TodayRelatedData(
      Subscription.findNewTodaySubscriptions.size,
      Subscription.findNewTodaySubscriptionsLastMonth.size,
      Subscription.findNewTodaySubscriptionsLastYear.size
    )
    val newStartsMTDData = MTDData(
      Subscription.findNewMTDSubscriptions.size,
      Subscription.findNewMTDSubscriptionsLastMonth.size,
      Subscription.findNewMTDSubscriptionsLastYear.size
    )
    val newStartsYTDData = YTDData(
      Subscription.findNewYTDSubscriptions.size,
      Subscription.findNewYTDSubscriptionsLastMonth.size,
      Subscription.findNewYTDSubscriptionsLastYear.size
    )

    val mtdShipments       = Shipment.findMtdShipments
    val mtdShipmentData    = MTDShipmentsData(mtdShipments.size, mtdShipments.sumAmountPaid)
    val todayShipments     = Shipment.findTodayShipments
    val todayShipmentsData = TodayShipmentsData(todayShipments.size, todayShipments.sumAmountPaid)

    val newUserCount       = Subscription.findNewMTDSubscriptions.size
    val cancellationsCount = Subscription.findCancelledMtdSubscriptions.size

    val remainingMonthSubscriptions: Int = Subscription.findCurrentMonthUpcomingSubscriptions.size
    ExecutiveDashboardReport(
      newStartsTodayData,
      newStartsMTDData,
      newStartsYTDData,
      mtdShipmentData,
      todayShipmentsData,
      remainingMonthSubscriptions,
      newUserCount,
      cancellationsCount
    )
  }

  def basicUsersUpgradeCsv: Box[InMemoryResponse] = {
    val data     = basicUsersUpgradeReport
    val fileName = s"user-upgrade-report-${LocalDate.now()}.csv"
    CSVHelper.inMemoryCsv(fileName, data)
  }

  def basicUsersUpgradeReport: BasicUsersUpgradeReport = {
    val upgradeReports =
      for {
        upgrade <- SubscriptionUpgrade.findAll
        subscription <- upgrade.subscription.obj.toList
        subscriptionStatus = subscription.status.get
      } yield {
        BasicUserUpgradeReport(
          upgrade.subscription.get,
          upgrade.user.get,
          subscriptionStatus,
          upgrade.shipmentCountAtUpgrade.get,
          upgrade.referrer.obj,
          upgrade.upgradeDate.get
        )
      }

    val activeUpgradesByAgency = upgradeReports
      .groupBy(_.agency)
      .map { case (agency, upgrades) =>
        val activeUpgrades = upgrades.filter { report =>
          List(Status.Active, Status.Paused).contains(report.subscriptionStatus)
        }

        val upgradeCounts = upgrades.map(_.shipmentCountAtUpgrade)
        val averageShipmentsBeforeUpgrade = BigDecimal(tryo(upgradeCounts.sum/upgradeCounts.size.toDouble).openOr(0D)).setScale(2, BigDecimal.RoundingMode.HALF_UP)

        val petCount = activeUpgrades
          .flatMap(u=> User.find(u.userId))
          .flatMap(_.pets)

        UpgradesByAgency(
          agency.map(_.name.get).getOrElse(""),
          upgrades.size,
          averageShipmentsBeforeUpgrade,
          activeUpgrades.size,
          petCount.size
        )
      }.toList

    val totalActiveUpgradedCount = activeUpgradesByAgency.map(_.activeUpgradeCount).sum
    val totalInactiveUpgradedCount = activeUpgradesByAgency.map(_.totalUpgradedCount).sum - totalActiveUpgradedCount

    BasicUsersUpgradeReport(
      totalActiveUpgradedCount,
      totalInactiveUpgradedCount,
      activeUpgradesByAgency
    )
  }

  def subscriptionRetentionCsv(
      lastPeriod: RetentionPeriod = RetentionPeriod.current(),
      periodsCount: Int = 12
  ): Box[InMemoryResponse] = {
    val data     = subscriptionRetentionReport(lastPeriod, periodsCount)
    val fileName = s"subscription-retention-${LocalDate.now()}.csv"
    CSVHelper.inMemoryCsv(fileName, data)
  }

  def snapshotInTimeCsv(
                         date: String
                       ): Box[InMemoryResponse] = {
    val data     = snapshotInTimeReport(date)
    val fileName = s"snapshot-in-time-$date.csv"
    CSVHelper.inMemoryCsv(fileName, data)
  }

  def customerLifespanCsv: Box[InMemoryResponse] = {
    val data     = customerLifespanReport
    val fileName = s"customer-lifespan-${LocalDate.now()}.csv"
    CSVHelper.inMemoryCsv(fileName, data)
  }

  def subscriptionRetentionReport(
      lastPeriod: RetentionPeriod = RetentionPeriod.current(),
      periodsCount: Int = 12
  ): SubscriptionRetentionReport = {
    val firstPeriod     = lastPeriod - (periodsCount - 1)
    val afterLastPeriod = lastPeriod.next
    val hwPriceCode = Props.get("default.price.code").openOr("")

    val subs = Subscription.findAll(
      By_>=(Subscription.startDate, firstPeriod.startDate),
      By_<(Subscription.startDate, afterLastPeriod.startDate),
      By(Subscription.priceCode, hwPriceCode)
    )

    val periods        = (0 until periodsCount).reverse.map(lastPeriod - _).toList
    val shipmentCounts = (1 to periodsCount).reverse
    val retentions =
      (periods zip shipmentCounts).map {
        case (period, shipmentCount) =>
          val startingSubs = subs
            .filter(s => RetentionPeriod.fromDate(s.startDate.get) == period)
            .filter(_.shipments.nonEmpty)

          SubscriptionRetentionForPeriod(
            period,
            startingSubs.size,
            shipmentCountsForPeriod(startingSubs, period, shipmentCount)
          )
      }

    SubscriptionRetentionReport(retentions)
  }

  private[service] def shipmentCountsForPeriod(
      subs: List[Subscription],
      period: RetentionPeriod,
      shipmentsCount: Int
  ): List[Int] = {
    val shipmentCountsByPeriod =
      filterMailedShipments(subs
        .flatMap(_.shipments))
        .groupBy(s => RetentionPeriod.fromDate(s.dateProcessed.get) - period)
        .mapValues(_.length)
        .withDefaultValue(0)

    (0 until shipmentsCount)
      .map(shipmentCountsByPeriod)
      .toList
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
      cancelsByMonth <- SubscriptionService.sameDayCancelsByMonth(subscriptions)
    } yield CancelsInMonthReport(cancelsByMonth._1, cancelsByMonth._2)
  }

  private[service] def agencyMonthSales(
      name: String,
      month: String,
      year: Int
  ): AgencyMonthSalesReport = {
    val totalUsers = Agency.getTotalUsers(name)

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
    val allSalesForMonthData = getAllSalesForMonthReport(shipmentMonthByAgent)
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

  private[service] def exportCancellationReport(agencyName: String): CancellationDataReport = {
    val possibleAgency     = Agency.find(By(Agency.name, agencyName))
    val customers          = possibleAgency.map(_.customers.toList).openOr(Nil)
    val cancelledCustomers = customers.filter(_.status.get == Status.Cancelled)

    val customerStatusTotals = customers.groupBy { customer =>
      customer.subscription.map(_.status.get)
    }.map { statusNCustomer =>
      CustomerStatusTotalsReport(statusNCustomer._1, statusNCustomer._2.size)
    }.toList

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

    val currentYearCancelSalesRow =
      CurrentYearSalesReport(
        currentYearSubscriptionCancels.size,
        averageShipmentsPerCancelByYear,
        currentYearCancelTotal,
        yearCancelCommisionAmount
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

    val currentMonthCancelSalesRow =
      CurrentMonthCancelSalesReport(
        currentMonthSubscriptionCancels.size,
        averageShipmentsPerCancelByMonth,
        currentMonthCancelTotal,
        monthCancelCommisionAmount
      )

    val allCancellationRows = getAllCancellationReport(customers)

    CancellationDataReport(
      customerStatusTotals,
      currentYearCancelSalesRow,
      currentMonthCancelSalesRow,
      allCancellationRows
    )
  }

  private[service] def calculateAgencyStats(agencyName: String, stats: List[StatisticsSnapshot]) = {
    StatsByAgency(
      agencyName,
      stats.map(_.subscriptionCount.get).sum,
      stats.map(_.petCount.get).sum
    )
  }

  private[service] def calculateStatsByProgram(stats: Iterable[StatisticsSnapshot]) = {
    stats
      .groupBy(_.program.get)
      .map { case (program, stats) =>
        (program, stats.map(_.subscriptionCount.get).sum, stats.map(_.petCount.get).sum)
      }
      .map { case (program, subscriptionCount, petCount) =>
        val programName = program.toString
        StatsByProgram(programName, subscriptionCount, petCount)
      }
  }

  private[service] def customerLifespanReport: CustomerLifespanReport = {
    val agencies = Agency.findAll(By(Agency.agencyType, AgencyType.Headquarters))

    agencies

    val subs = Subscription.findAll().groupBy(_.status)
    val users = User.findAll().groupBy(_.referer)
    subs
    users

    val report = agencies.flatMap { agency =>
      val customers = agency.customers.toList
      val subscriptions = customers.flatMap(_.subscription.obj)
      val subscriptionsGrouped = subscriptions.groupBy(_.isActive)
      subscriptionsGrouped

      val lifespanStatistics = subscriptionsGrouped.map { case (isActive, subscriptions) =>
        val status = if (isActive) "Active" else "Inactive"

        val foo = subscriptions.groupBy { subscription =>
          val startDate = subscription.startDate.get
          val endDate = if(isActive) Empty else tryo(subscription.cancellationDate.get)

          calculateLifespan(startDate, endDate)
        }

        val subscriptionGroupedByLifespan = foo.mapValues(_.length).withDefaultValue(0)

        subscriptionGroupedByLifespan

        status ->
          LifespanStatistics(
            subscriptionGroupedByLifespan(ZeroFourMonths),
            subscriptionGroupedByLifespan(FourSixMonths),
            subscriptionGroupedByLifespan(SixTwelveMonths),
            subscriptionGroupedByLifespan(OneTwoYears),
            subscriptionGroupedByLifespan(TwoThreeYears),
            subscriptionGroupedByLifespan(ThreePlusYears),
          )
      }

      val foo = lifespanStatistics.map { case (status, statistics) =>
        LifespanByAgency(
          agency.name.get,
          status,
          statistics,
          LifespanStatistics(0,0,0,0,0,0)
        )
      }

      foo
    }
    CustomerLifespanReport(report)
  }

  private def calculateLifespan(startDate: Date, endDate: Box[Date]) = {
    val monthDiff = getMonthDiff(startDate, endDate)

    monthDiff match {
      case diff if diff >= 0 && diff < 4   => ZeroFourMonths
      case diff if diff >= 4 && diff < 6   => FourSixMonths
      case diff if diff >= 6 && diff < 12  => SixTwelveMonths
      case diff if diff >= 12 && diff < 24 => OneTwoYears
      case diff if diff >= 24 && diff < 36 => TwoThreeYears
      case diff if diff >= 36              => ThreePlusYears
    }
  }

  private[service] def snapshotInTimeReport(rawDate: String): SnapshotInTimeReport = {
    val date = DateHelper.dateFormat.parse(rawDate)
    val datePlusOne = DateHelper.datePlusDays(date, 1)

    val snapshotStatistics = StatisticsSnapshot.findAll(
      By_>=(StatisticsSnapshot.date, date),
      By_<(StatisticsSnapshot.date, datePlusOne)
    )

    val totalSubscriptions = snapshotStatistics.map(_.subscriptionCount.get).sum
    val totalPets = snapshotStatistics.map(_.petCount.get).sum
    val petsByProgram = calculateStatsByProgram(snapshotStatistics)

    val mpdAgency = Agency.mpdAgency
    val tppAgency = Agency.tppAgency

    val mpdAgencyName = mpdAgency.map(_.name.get).openOr("")
    val tppAgencyName = tppAgency.map(_.name.get).openOr("")

    val mpdId = mpdAgency.map(_.id.get).openOr(0L)
    val petsByAgency = snapshotStatistics
      .partition(_.agency.get == mpdId)

    val mpdStats = petsByAgency._1
    val tppStats = petsByAgency._2

    val mpdCalculated = calculateAgencyStats(mpdAgencyName, mpdStats)
    val tppCalculated = calculateAgencyStats(tppAgencyName, tppStats)

    val mpdStatsByProgram = calculateStatsByProgram(mpdStats)
    val tppStatsByProgram = calculateStatsByProgram(tppStats)

    val mpdStatsByProgramByAgency = StatsByProgramByAgency(mpdAgencyName, mpdStatsByProgram)
    val tppStatsByProgramByAgency = StatsByProgramByAgency(tppAgencyName, tppStatsByProgram)

    SnapshotInTimeReport(
      totalSubscriptions,
      totalPets,
      petsByProgram,
      List(mpdCalculated, tppCalculated),
      List(mpdStatsByProgramByAgency, tppStatsByProgramByAgency)
    )
  }

  private[service] def executiveSnapshotReport: ExecutiveSnapshotReport = {
    val allActiveSubs         = Subscription.activeSubscriptions
    val allActiveUpgradedSubs = Subscription.upgradedActiveSubscriptions
    val upgradedCancelledSubs = Subscription.upgradedAndCancelledSubscriptions
    val allActivePets         = allActiveSubs.getAllActivePets
    val allActiveUpgradedBoxes = allActiveUpgradedSubs
      .flatMap(_.subscriptionBoxes.filter(_.boxType == BoxType.healthAndWellness))
      .filter(_.status.get == Status.Active)
    val allAccountsReport     = AllAccountsReport(allActiveSubs.size, allActivePets.size)
    val upgradedSubsReport =
      upgradedSubscriptionsReport(
        allActiveUpgradedSubs,
        allActiveUpgradedBoxes,
        upgradedCancelledSubs
      )
    val activeUpgradedPetsBySize    = countPetsByProduct(allActiveUpgradedSubs)
    val cancelledUpgradedPetsBySize = countPetsByProduct(upgradedCancelledSubs)
    val upgradedNCancelledSubsByPetsCount = cancelledUpgradedSubscriptionsByPetCount(
      upgradedCancelledSubs
    )
    val upgradedNCancelledSubsByShipmentsCount = cancelledUpgradedSubscriptionsByShipmentCount(
      upgradedCancelledSubs
    )
    val activeUpgradesByAgency   = countUpgradesByAgency(allActiveUpgradedSubs)
    val canceledUpgradesByAgency = countUpgradesByAgency(upgradedCancelledSubs)
    ExecutiveSnapshotReport(
      allAccountsReport,
      upgradedSubsReport,
      activeUpgradedPetsBySize,
      cancelledUpgradedPetsBySize,
      upgradedNCancelledSubsByPetsCount,
      upgradedNCancelledSubsByShipmentsCount,
      activeUpgradesByAgency,
      canceledUpgradesByAgency
    )
  }

  private def cancelledUpgradedSubscriptionsByShipmentCount(
      subs: List[Subscription]
  ): Iterable[CancelledUpgradedSubscriptionsByCount] =
    CalculationHelper
      .calculateOccurrences[Int, Subscription](subs, _.shipments.toList.size)
      .map(CancelledUpgradedSubscriptionsByCount.tupled)

  private def cancelledUpgradedSubscriptionsByPetCount(
      subs: List[Subscription]
  ): Iterable[CancelledUpgradedSubscriptionsByCount] = {
    val petsSizes = subs
      .flatMap(_.user.toOption.map(_.pets.toList.size))
    CalculationHelper
      .calculateOccurrences[Int, Int](petsSizes, identity)
      .map(CancelledUpgradedSubscriptionsByCount.tupled)
  }

  private def countPetsByProduct(subs: List[Subscription]): Iterable[PetsByProduct] = {
    val fleaTick = subs.flatMap(_.subscriptionBoxes).flatMap(_.fleaTick.toList)
    CalculationHelper
      .calculateOccurrences[String, FleaTick](fleaTick, _.getNameAndSize)
      .map(PetsByProduct.tupled)
  }

  private def countUpgradesByAgency(
      canceledSubs: List[Subscription]
  ): Iterable[CountedByAgency] = {
    val subscriptionsAgencies = canceledSubs
      .flatMap(_.user.flatMap(_.referer))
      .map(Agency.getHQFor)
      .filter { agency =>
        val agencyName = agency.name.get
        agencyName == myPetDefenseName || agencyName == tppName
      }
    CalculationHelper
      .calculateOccurrences[String, Agency](subscriptionsAgencies, _.name.get)
      .map(CountedByAgency.tupled)
  }

  private def upgradedSubscriptionsReport(
      allActiveUpgradedSubs: List[Subscription],
      allActiveBoxes: List[SubscriptionBox],
      upgradedCancelledSubs: List[Subscription]
  ): UpgradedSubscriptionsReport =
    UpgradedSubscriptionsReport(
      allActiveUpgradedSubs.size,
      allActiveBoxes.size,
      upgradedCancelledSubs.size
    )

  private def getAllSalesForMonthReport(
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

  private def getAllCancellationReport(customers: List[User]): List[AllCancellationReport] =
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

      AllCancellationReport(
        customer.userId.get,
        firstShipmentDate.getMonth,
        lastShipmentDate.getMonth,
        subscription.status.get,
        shipmentCount,
        totalGrossSales,
        totalCommission
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

  def getActiveSubscriptionsByAgency: Map[Agency, List[Subscription]] = {
    val activeSubscriptions = Subscription.activeSubscriptions

    val agencyAndSubscriptions = for {
      sub <- activeSubscriptions
      user <- sub.user.obj
      agency <- user.referer.obj
    } yield (agency, sub)

    agencyAndSubscriptions.groupBy(_._1).collect { case (agency, agencySubs) =>
      agency -> agencySubs.map(_._2)
    }
  }

  def getSnapshotStatisticsForSubsByAgency(subsByAgency:  Map[Agency, List[Subscription]]): Iterable[SnapshotStatistics] = {
    for {
      (agency, subscriptions) <- subsByAgency
      activeBoxes = subscriptions
        .flatMap(_.subscriptionBoxes)
        .filter(_.status.get == Status.Active)
      (boxType, boxes) <- activeBoxes.groupBy(_.boxType.get)
      subIds = boxes.map(_.subscription.get).distinct
      boxStatistics = BoxStatistics(boxType, boxes.size, subIds.size)
    } yield {
      SnapshotStatistics(agency, boxStatistics)
    }
  }
}
