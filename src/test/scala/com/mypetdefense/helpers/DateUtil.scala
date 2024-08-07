package com.mypetdefense.helpers

import java.time._
import java.util.Date

import com.mypetdefense.helpers.Random.generateIntBetween

import scala.annotation.tailrec

object DateUtil {

  val zoneId: ZoneId = ZoneId.of("America/New_York")

  // always defined
  def threeDaysAgo: ZonedDateTime                           = now.atStartOfDay(zoneId).minusDays(3)
  def yesterday: ZonedDateTime                              = now.atStartOfDay(zoneId).minusDays(1)
  def firstDayOfNextMonth: ZonedDateTime                    = now.atStartOfDay(zoneId).plusMonths(1).withDayOfMonth(1)
  def now: LocalDate                                        = LocalDate.now(zoneId)
  def today: Date                                           = now.atStartOfDay(zoneId).toDate
  def tomorrow: Date                                        = now.atStartOfDay(zoneId).plusDays(1).toDate
  def thisYear: Int                                         = now.getYear
  def anyHourOfYesterday: ZonedDateTime                     = anyHourOf(yesterday.toLocalDate)
  def anyHourOfToday: ZonedDateTime                         = anyHourOf(now)
  def anyHourOfThisDayMonthAgo: ZonedDateTime               = anyHourOf(now.minusMonths(1))
  def anyHourOfThisDayYearAgo: ZonedDateTime                = anyHourOf(now.minusYears(1))
  def anyDayOfLastMonth: ZonedDateTime                      = getAnyDayOfMonth(now.minusMonths(1))
  def anyDayOfThisMonth: ZonedDateTime                      = getAnyDayOfMonth(now)
  def anyDayOfNextMonth: ZonedDateTime                      = getAnyDayOfMonth(now.plusMonths(1))
  def anyDayOfThisYear: ZonedDateTime                       = getAnyDayOfTheYear(now)
  def anyDayOfNext19Days: ZonedDateTime                     = plusRangeDays(now, 1, 19)
  def anyDayOFromPlus21Days: ZonedDateTime                  = getAnyDayOfYearFrom(now.plusDays(21))
  def anyDayUntilLastMonth: ZonedDateTime                   = getAnyDayOfTheYearUntil(now.minusMonths(2))
  def anyDayUntilThisMonth: ZonedDateTime                   = getAnyDayOfTheYearUntil(now.minusMonths(1))
  def anyDayUntilToday: ZonedDateTime                       = getAnyDayOfTheYearUntil(now.minusDays(1))
  def anyDayExceptToday: ZonedDateTime                      = getAnyDayOfTheYearExceptDay(now)
  def anyDayExceptYesterday: ZonedDateTime                  = getAnyDayOfTheYearExceptDay(now.minusDays(1))
  def anyDayExceptThisDayMonthAgo: ZonedDateTime            = getAnyDayOfTheYearExceptDay(now.minusMonths(1))
  def anyDayExceptThisDayYearAgo: ZonedDateTime             = getAnyDayOfTheYearExceptDay(now.minusYears(1))
  def anyDayExceptThisMonth: ZonedDateTime                  = getAnyDayOfTheYearExceptMonth(now.minusYears(1))
  def anyDayOfLastYear: ZonedDateTime                       = getAnyDayOfTheYear(now.minusYears(1))
  def anyDayOfLastMonthUntilMonthEnd: ZonedDateTime         = getAnyDayOfTheMonthUntil(now.minusMonths(1))
  def anyDayUntilSixtyDaysAgo: ZonedDateTime                = getAnyDayOfTheYearUntil(now.minusDays(60))
  def anyDayOfLastYearThisDay: ZonedDateTime                = getAnyDayOfTheMonthUntil(now.minusYears(1))
  def validDateForGetCurrentPastDueShipments: ZonedDateTime = minusRangeDays(now.plusDays(1), 1, 20)
  def validDateForGetPastDueShipments: ZonedDateTime        = minusRangeDays(now, 6, 9)
  def invalidDateForGetPastDueShipments: ZonedDateTime      = minusRangeDays(now.plusDays(1), 1, 5)
  def inReportForecastDefaultRange: ZonedDateTime           = now.plusDays(14).atStartOfDay(zoneId)
  def notInReportForecastDefaultRange: ZonedDateTime        = getAnyDayOfTheYearExceptDay(now.plusDays(14))

  def twoMonthsAgo: ZonedDateTime = {
    val nowDate = now
    nowDate.minusMonths(2).atStartOfDay(zoneId)
  }

  def eightMonthsAgo: ZonedDateTime = {
    val nowDate = now
    nowDate.minusMonths(8).atStartOfDay(zoneId)
  }

  def lastYear: ZonedDateTime = {
    val nowDate = now
    nowDate.minusYears(1).atStartOfDay(zoneId)
  }

  def twoYearAgo: ZonedDateTime = {
    val nowDate = now
    nowDate.minusYears(2).atStartOfDay(zoneId)
  }

  def threeYearAgo: ZonedDateTime = {
    val nowDate = now
    nowDate.minusYears(3).atStartOfDay(zoneId)
  }

  // not always defined
  def anyDayOfThisMonthFromTomorrow: Option[ZonedDateTime] =
    getAnyDayOfMonthFrom(now.plusDays(1))
      .when(now.plusDays(1).getMonth == now.getMonth)
  def anyDayOfThisYearUntilThisMonth: Option[ZonedDateTime] =
    getAnyDayOfTheYearUntil(now.withDayOfMonth(1).minusDays(1))
      .when(now.withDayOfMonth(1).minusDays(1).getYear == now.getYear)
  def anyDayOfThisYearUntilMonthAgo: Option[ZonedDateTime] =
    getAnyDayOfTheYearUntil(now.minusMonths(1))
      .when(now.minusMonths(1).getYear == now.getYear)
  def anyDayOfThisYearFromMonthAgo: Option[ZonedDateTime] =
    getAnyDayOfYearFrom(now.minusMonths(1).plusDays(1))
      .when(now.minusMonths(1).plusDays(1).getYear == now.getYear)
  def anyDayOfLastYearFromThisDayYearAgo: Option[ZonedDateTime] =
    getAnyDayOfYearFrom(now.plusDays(1).minusYears(1))
      .when(now.plusDays(1).minusYears(1).getYear == now.getYear - 1)

  protected def anyHourOf(localDate: LocalDate): ZonedDateTime = {
    val random = generateIntBetween(0, 23)
    localDate.atStartOfDay(zoneId).withHour(random)
  }

  @tailrec
  protected def getAnyDayOfTheYearExceptDay(localDate: LocalDate): ZonedDateTime = {
    val maybeDay = getAnyDayOfTheYear(localDate)
    if (localDate.getDayOfYear == maybeDay.getDayOfYear)
      getAnyDayOfTheYearExceptDay(localDate)
    else
      maybeDay
  }

  @tailrec
  protected def getAnyDayOfTheYearExceptMonth(localDate: LocalDate): ZonedDateTime = {
    val maybeDay = getAnyDayOfTheYear(localDate)
    if (localDate.getMonth == maybeDay.getMonth)
      getAnyDayOfTheYearExceptMonth(localDate)
    else
      maybeDay
  }

  protected def plusRangeDays(
      localDate: LocalDate,
      rangeStart: Int,
      rangeEnd: Int
  ): ZonedDateTime = {
    val day = generateIntBetween(rangeStart, rangeEnd)
    localDate.plusDays(day).atStartOfDay(zoneId)
  }

  protected def minusRangeDays(
      localDate: LocalDate,
      rangeStart: Int,
      rangeEnd: Int
  ): ZonedDateTime = {
    val day = generateIntBetween(rangeStart, rangeEnd)
    localDate.minusDays(day).atStartOfDay(zoneId)
  }

  protected def getAnyDayOfTheMonthUntil(localDate: LocalDate): ZonedDateTime = {
    val maxDays = localDate.getDayOfMonth
    val day     = generateIntBetween(1, maxDays)
    localDate.withDayOfMonth(day).atStartOfDay(zoneId)
  }

  protected def getAnyDayOfTheYearUntil(localDate: LocalDate): ZonedDateTime = {
    val maxDays = localDate.getDayOfYear
    val day     = generateIntBetween(1, maxDays)
    localDate.withDayOfYear(day).atStartOfDay(zoneId)
  }

  protected def getAnyDayOfMonthFrom(localDate: LocalDate): ZonedDateTime = {
    val maxDays = localDate.lengthOfMonth()
    val dayFrom = localDate.getDayOfMonth
    val day     = generateIntBetween(dayFrom, maxDays)
    localDate.withDayOfMonth(day).atStartOfDay(zoneId)
  }

  protected def getAnyDayOfYearFrom(localDate: LocalDate): ZonedDateTime = {
    val maxDays = localDate.lengthOfYear()
    val dayFrom = localDate.getDayOfYear
    val day     = generateIntBetween(dayFrom, maxDays)
    localDate.withDayOfYear(day).atStartOfDay(zoneId)
  }

  protected def getAnyDayOfTheYear(localDate: LocalDate): ZonedDateTime = {
    val maxDays = localDate.lengthOfYear
    val day     = generateIntBetween(1, maxDays)
    localDate.withDayOfYear(day).atStartOfDay(zoneId)
  }

  protected def getAnyDayOfMonth(localDate: LocalDate): ZonedDateTime = {
    val maxDays = localDate.lengthOfMonth()
    val day     = generateIntBetween(1, maxDays)
    localDate.withDayOfMonth(day).atStartOfDay(zoneId)
  }

  implicit class ZonedDateTimeSyntax(val i: ZonedDateTime) extends AnyVal {
    def toDate: Date = Date.from(i.toInstant)
  }

  private implicit class AnyOps[T](self: => T) {
    def when(cond: Boolean): Option[T] = if (cond) Some(self) else None
  }

}
