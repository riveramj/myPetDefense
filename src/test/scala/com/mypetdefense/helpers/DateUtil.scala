package com.mypetdefense.helpers

import java.time._
import java.util.Date

import com.mypetdefense.helpers.Random.generateIntBetween

import scala.annotation.tailrec

object DateUtil {

  val zoneId: ZoneId = ZoneId.of("America/New_York")

  def yesterday: ZonedDateTime                      = now.atStartOfDay(zoneId).minusDays(1)
  def now: LocalDate                                = LocalDate.now(zoneId)
  def today: Date                                   = now.atStartOfDay(zoneId).toDate
  def tomorrow: Date                                = now.atStartOfDay(zoneId).plusDays(1).toDate
  def thisYear: Int                                 = now.getYear
  def anyHourOfYesterday: ZonedDateTime             = anyHourOf(yesterday.toLocalDate)
  def anyHourOfToday: ZonedDateTime                 = anyHourOf(now)
  def anyHourOfThisDayMonthAgo: ZonedDateTime       = anyHourOf(now.minusMonths(1))
  def anyHourOfThisDayYearAgo: ZonedDateTime        = anyHourOf(now.minusYears(1))
  def anyDayOfYesterdayMonth: ZonedDateTime         = getAnyDayOfMonth(now.minusDays(1))
  def anyDayOfLastMonth: ZonedDateTime              = getAnyDayOfMonth(now.minusMonths(1))
  def anyDayOfThisMonth: ZonedDateTime              = getAnyDayOfMonth(now)
  def anyDayOfNextMonth: ZonedDateTime              = getAnyDayOfMonth(now.plusMonths(1))
  def anyDayOfThisYear: ZonedDateTime               = getAnyDayOfTheYear(now)
  def anyDayUntilLastMonth: ZonedDateTime           = getAnyDayOfTheYearUntil(now.minusMonths(2))
  def anyDayUntilThisMonth: ZonedDateTime           = getAnyDayOfTheYearUntil(now.minusMonths(1))
  def anyDayUntilToday: ZonedDateTime               = getAnyDayOfTheYearUntil(now.minusDays(1))
  def anyDayOfThisMonthFromTomorrow: ZonedDateTime  = getAnyDayOfMonthFrom(now.plusDays(1))
  def anyDayExceptToday: ZonedDateTime              = getAnyDayOfTheYearExceptDay(now)
  def anyDayExceptYesterday: ZonedDateTime          = getAnyDayOfTheYearExceptDay(now.minusDays(1))
  def anyDayExceptThisDayMonthAgo: ZonedDateTime    = getAnyDayOfTheYearExceptDay(now.minusMonths(1))
  def anyDayExceptThisDayYearAgo: ZonedDateTime     = getAnyDayOfTheYearExceptDay(now.minusYears(1))
  def anyDayExceptThisMonth: ZonedDateTime          = getAnyDayOfTheYearExceptMonth(now.minusYears(1))
  def anyDayOfLastYear: ZonedDateTime               = getAnyDayOfTheYear(now.minusYears(1))
  def anyDayOfLastMonthUntilMonthEnd: ZonedDateTime = getAnyDayOfTheMonthUntil(now.minusMonths(1))
  def anyDayOfThisYearUntilMonthAgo: ZonedDateTime  = getAnyDayOfTheYearUntil(now.minusMonths(1))
  def anyDayOfThisYearFromMonthAgo: ZonedDateTime =
    getAnyDayOfYearFrom(now.minusMonths(1).plusDays(1))
  def anyDayOfLastYearThisDay: ZonedDateTime =
    getAnyDayOfTheMonthUntil(now.minusYears(1))

  def anyDayOfLastYearFromThisDayYearAgo: ZonedDateTime =
    getAnyDayOfYearFrom(now.plusDays(1).minusYears(1))

  def lastYear: ZonedDateTime = {
    val nowDate = now
    nowDate.minusYears(1).atStartOfDay(zoneId)
  }

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

}
