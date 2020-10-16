package com.mypetdefense.helpers

import java.time._
import java.util.Date

import com.mypetdefense.helpers.Random.generateIntBetween

object DateUtil {

  val zoneId: ZoneId = ZoneId.of("America/New_York")

  def currentDate: LocalDateTime = LocalDateTime.now()
  def now: LocalDate             = LocalDate.now(zoneId)
  def today: Date                = now.atStartOfDay(zoneId).toDate
  def tomorrow: Date             = now.atStartOfDay(zoneId).plusDays(1).toDate
  def lastMonth: Date            = now.minusMonths(1).atStartOfDay(zoneId).toDate
  def thisMonth: Date = {
    val nowDate = now
    val maxDays = nowDate.lengthOfMonth()
    val day     = generateIntBetween(1, maxDays)
    nowDate.withDayOfMonth(day).atStartOfDay(zoneId).toDate
  }

  def lastYear: Date = {
    val nowDate = now
    nowDate.minusYears(1).atStartOfDay(zoneId).toDate
  }

  implicit class ZonedDateTimeSyntax(val i: ZonedDateTime) extends AnyVal {
    def toDate: Date = Date.from(i.toInstant)
  }

}
