package com.mypetdefense.helpers

import java.time._
import java.util.Date

import com.mypetdefense.helpers.Random.generateIntBetween

object DateUtil {

  val zoneId: ZoneId = ZoneId.of("America/New_York")

  def now: LocalDate                   = LocalDate.now(zoneId)
  def today: Date                      = now.atStartOfDay(zoneId).toDate
  def tomorrow: Date                   = now.atStartOfDay(zoneId).plusDays(1).toDate
  def anyDayOfThisMonth: ZonedDateTime = getAnyDayOfMonth(now)
  def anyDayOfLastMonth: ZonedDateTime = getAnyDayOfMonth(now.minusMonths(1))

  def lastYear: Date = {
    val nowDate = now
    nowDate.minusYears(1).atStartOfDay(zoneId).toDate
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
