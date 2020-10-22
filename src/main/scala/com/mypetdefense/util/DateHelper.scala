package com.mypetdefense.util

import java.text.SimpleDateFormat
import java.time.{LocalDate, LocalDateTime, YearMonth, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.{Date, Locale}

import com.mypetdefense.service.ReportingService.convertMonthToDate
import net.liftweb.util.Helpers.tryo

object DateHelper {
  val signupCancelDateFormat = new SimpleDateFormat("MM/dd/yyyy")
  val zoneId: ZoneId         = ZoneId.of("America/New_York")

  def currentDate: LocalDateTime     = LocalDateTime.now()
  def now: LocalDate                 = LocalDate.now(zoneId)
  def nowAtStartOfDay: ZonedDateTime = now.atStartOfDay(zoneId)

  def nowDate: Date = Date.from(nowAtStartOfDay.toInstant)

  def yesterday: ZonedDateTime = nowAtStartOfDay.minusDays(1)

  def yesterdayStart: Date = Date.from(nowAtStartOfDay.minusDays(1).toInstant)

  def yesterdayEnd: Date = Date.from(nowAtStartOfDay.toInstant)

  def yesterdayMonthStart: Date =
    Date.from(yesterday.withDayOfMonth(1).toInstant)

  def yesterdayMonthEnd: Date =
    Date.from(lastDayOfTheMonth(yesterday).plusDays(1).toInstant)

  def monthDayOne: Date = Date.from(now.withDayOfMonth(1).atStartOfDay(zoneId).toInstant)

  def monthDayOneLastMonth: Date =
    Date.from(now.withDayOfMonth(1).atStartOfDay(zoneId).minusMonths(1).toInstant)

  def currentDayLastMonthEnd: Date = Date.from(nowAtStartOfDay.plusDays(1).minusMonths(1).toInstant)

  def monthDayOneLastYear: Date =
    Date.from(now.withDayOfMonth(1).atStartOfDay(zoneId).minusYears(1).toInstant)

  def currentDayLastYearEnd: Date = Date.from(nowAtStartOfDay.plusDays(1).minusYears(1).toInstant)

  def tomorrowStart: Date = Date.from(nowAtStartOfDay.plusDays(1).toInstant)

  def beginngNextMonth: Date =
    Date.from(YearMonth.now().atEndOfMonth().atStartOfDay(zoneId).plusDays(1).toInstant)

  def yearDayOne: Date = Date.from(now.withDayOfYear(1).atStartOfDay(zoneId).toInstant)

  def yearDayOneLastYear: Date =
    Date.from(now.withDayOfYear(1).atStartOfDay(zoneId).minusYears(1).toInstant)

  def todayLastMonth: Date = Date.from(nowAtStartOfDay.minusMonths(1).toInstant)

  def todayLastYear: Date = Date.from(nowAtStartOfDay.minusYears(1).toInstant)

  def todayLastYearEnd: Date = Date.from(nowAtStartOfDay.minusYears(1).plusDays(1).toInstant)

  def todayLastMonthEnd: Date = Date.from(nowAtStartOfDay.minusMonths(1).plusDays(1).toInstant)

  def yearMonth: String =
    currentDate.format(DateTimeFormatter.ofPattern("MMMM yyyy", Locale.ENGLISH))

  def fileNameYearMonth: String =
    currentDate.format(DateTimeFormatter.ofPattern("MMMyyyy", Locale.ENGLISH))

  def fileNameMonthDayYear: String =
    currentDate.format(DateTimeFormatter.ofPattern("MM-dd-yyyy", Locale.ENGLISH))

  def thisYear: String = currentDate.format(DateTimeFormatter.ofPattern("yyyy", Locale.ENGLISH))

  def lastDayOfTheMonth(time: ZonedDateTime): ZonedDateTime = {
    val maxDay = time.getMonth.maxLength()
    time.withDayOfMonth(maxDay)
  }

  def getDateRange(month: String, year: Int = 2019): LocalDateTime = {
    if (month == "") {
      currentDate
    } else {
      convertMonthToDate(month, year)
    }
  }

  def getYearOrCurrent(year: String): Int = {
    if (year == "") {
      currentDate.getYear
    } else {
      tryo(year.toInt).openOr(0)
    }
  }

  val monthHeaders: List[String] =
    List(
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December"
    )
}
