package com.mypetdefense.helpers

import java.time._
import java.util.Date

import com.mypetdefense.helpers.Random.generateIntBetween

import scala.annotation.tailrec

object DateUtil {

  val zoneId: ZoneId = ZoneId.of("America/New_York")

  def yesterday: ZonedDateTime                     = now.atStartOfDay(zoneId).minusDays(1)
  def now: LocalDate                               = LocalDate.now(zoneId)
  def today: Date                                  = now.atStartOfDay(zoneId).toDate
  def tomorrow: Date                               = now.atStartOfDay(zoneId).plusDays(1).toDate
  def thisYear: Int                                = now.getYear
  def anyHourOfYesterday: ZonedDateTime            = anyHourOf(yesterday.toLocalDate)
  def anyHourOfToday: ZonedDateTime                = anyHourOf(now)
  def anyDayOfLastMonth: ZonedDateTime             = getAnyDayOfMonth(now.minusMonths(1))
  def anyDayOfThisMonth: ZonedDateTime             = getAnyDayOfMonth(now)
  def anyDayOfNextMonth: ZonedDateTime             = getAnyDayOfMonth(now.plusMonths(1))
  def anyDayOfThisYear: ZonedDateTime              = getAnyDayOfTheYear(now)
  def anyDayUntilThisMonth: ZonedDateTime          = getAnyDayOfTheYearUntil(now.minusMonths(1))
  def anyDayUntilToday: ZonedDateTime              = getAnyDayOfTheYearUntil(now.minusDays(1))
  def anyDayOfThisMonthFromTomorrow: ZonedDateTime = getAnyDayOfMonthFrom(now.plusDays(1))
  def anyDayExceptToday: ZonedDateTime             = getAnyDayOfTheYearExcept(now)
  def anyDayExceptYesterday: ZonedDateTime         = getAnyDayOfTheYearExcept(now.minusDays(1))
  def anyDayOfLastYear: ZonedDateTime              = getAnyDayOfTheYear(now.minusYears(1))

  def lastYear: ZonedDateTime = {
    val nowDate = now
    nowDate.minusYears(1).atStartOfDay(zoneId)
  }

  protected def anyHourOf(localDate: LocalDate): ZonedDateTime = {
    val random = generateIntBetween(0, 23)
    localDate.atStartOfDay(zoneId).withHour(random)
  }

  @tailrec
  protected def getAnyDayOfTheYearExcept(localDate: LocalDate): ZonedDateTime = {
    val maybeDay = getAnyDayOfTheYear(localDate)
    if (localDate.getDayOfYear == maybeDay.getDayOfYear)
      getAnyDayOfTheYearExcept(localDate)
    else
      maybeDay
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
