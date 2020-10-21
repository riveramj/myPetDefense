package com.mypetdefense.helpers

import java.time._
import java.util.Date

import com.mypetdefense.helpers.Random.generateIntBetween

import scala.annotation.tailrec

object DateUtil {

  val zoneId: ZoneId = ZoneId.of("America/New_York")

  def yesterday: ZonedDateTime             = now.atStartOfDay(zoneId).minusDays(1)
  def now: LocalDate                       = LocalDate.now(zoneId)
  def today: Date                          = now.atStartOfDay(zoneId).toDate
  def tomorrow: Date                       = now.atStartOfDay(zoneId).plusDays(1).toDate
  def thisYear: Int                        = now.getYear
  def anyHourOfYesterday: ZonedDateTime    = anyHourOf(yesterday.toLocalDate)
  def anyDayOfThisMonth: ZonedDateTime     = getAnyDayOfMonth(now)
  def anyDayOfLastMonth: ZonedDateTime     = getAnyDayOfMonth(now.minusMonths(1))
  def anyDayOfThisYear: ZonedDateTime      = getAnyDayOfTheYear(now)
  def anyDayUntilThisMonth: ZonedDateTime  = getAnyDayOfTheYearUntil(now.minusMonths(1))
  def anyDayExceptYesterday: ZonedDateTime = getAnyDayOfTheYearExceptYesterday(now)
  def anyDayOfLastYear: ZonedDateTime      = getAnyDayOfTheYear(now.minusYears(1))

  def lastYear: ZonedDateTime = {
    val nowDate = now
    nowDate.minusYears(1).atStartOfDay(zoneId)
  }

  protected def anyHourOf(localDate: LocalDate): ZonedDateTime = {
    val random = generateIntBetween(0, 23)
    localDate.atStartOfDay(zoneId).withHour(random)
  }

  @tailrec
  protected def getAnyDayOfTheYearExceptYesterday(localDate: LocalDate): ZonedDateTime = {
    val yesterday = localDate.minusDays(1)
    val maybeDay  = getAnyDayOfTheYear(localDate)
    if (yesterday.getDayOfYear == maybeDay.getDayOfYear)
      getAnyDayOfTheYearExceptYesterday(localDate)
    else
      maybeDay
  }

  protected def getAnyDayOfTheYearUntil(localDate: LocalDate): ZonedDateTime = {
    val maxDays = localDate.getDayOfYear
    val day     = generateIntBetween(1, maxDays)
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
