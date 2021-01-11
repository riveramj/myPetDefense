package com.mypetdefense.util

import java.text.SimpleDateFormat
import java.time._
import java.time.format.DateTimeFormatter
import java.util.Date

import com.mypetdefense.AppConstants.{DefaultLocale, DefaultTimezone}
import net.liftweb.util.Helpers.tryo

object DateHelper {
  val dateFormat = new SimpleDateFormat("MM/dd/yyyy")

  def currentDate: LocalDateTime     = LocalDateTime.now()
  def now: LocalDate                 = LocalDate.now(DefaultTimezone)
  def nowAtStartOfDay: ZonedDateTime = now.atStartOfDay(DefaultTimezone)

  def nowDate: Date = Date.from(nowAtStartOfDay.toInstant)

  def yesterday: ZonedDateTime = nowAtStartOfDay.minusDays(1)

  def yesterdayStart: Date = Date.from(nowAtStartOfDay.minusDays(1).toInstant)

  def yesterdayEnd: Date = Date.from(nowAtStartOfDay.toInstant)

  def yesterdayMonthStart: Date =
    Date.from(yesterday.withDayOfMonth(1).toInstant)

  def yesterdayMonthEnd: Date =
    Date.from(lastDayOfTheMonth(yesterday).plusDays(1).toInstant)

  def threeDaysAgo: Date = Date.from(now.minusDays(3).atStartOfDay(DefaultTimezone).toInstant)

  def sixtyDaysAgo: Date = Date.from(now.minusDays(60).atStartOfDay(DefaultTimezone).toInstant)

  def monthDayOne: Date = Date.from(now.withDayOfMonth(1).atStartOfDay(DefaultTimezone).toInstant)

  def monthDayOneLastMonth: Date =
    Date.from(now.withDayOfMonth(1).atStartOfDay(DefaultTimezone).minusMonths(1).toInstant)

  def currentDayLastMonthEnd: Date = Date.from(nowAtStartOfDay.withDayOfMonth(1).toInstant)

  def monthDayOneLastYear: Date =
    Date.from(now.withDayOfMonth(1).atStartOfDay(DefaultTimezone).minusYears(1).toInstant)

  def currentDayLastYearEnd: Date = Date.from(nowAtStartOfDay.plusDays(1).minusYears(1).toInstant)

  def tomorrowStart: Date = Date.from(nowAtStartOfDay.plusDays(1).toInstant)

  def beginngNextMonth: Date =
    Date.from(YearMonth.now().atEndOfMonth().atStartOfDay(DefaultTimezone).plusDays(1).toInstant)

  def yearDayOne: Date = Date.from(now.withDayOfYear(1).atStartOfDay(DefaultTimezone).toInstant)

  def yearDayOneLastYear: Date =
    Date.from(now.withDayOfYear(1).atStartOfDay(DefaultTimezone).minusYears(1).toInstant)

  def todayLastMonth: Date = Date.from(nowAtStartOfDay.minusMonths(1).toInstant)

  def todayLastYear: Date = Date.from(nowAtStartOfDay.minusYears(1).toInstant)

  def todayLastYearEnd: Date = Date.from(nowAtStartOfDay.minusYears(1).plusDays(1).toInstant)

  def todayLastMonthEnd: Date = Date.from(nowAtStartOfDay.minusMonths(1).plusDays(1).toInstant)

  def yearMonth: String =
    currentDate.format(DateTimeFormatter.ofPattern("MMMM yyyy", DefaultLocale))

  def fileNameYearMonth: String =
    currentDate.format(DateTimeFormatter.ofPattern("MMMyyyy", DefaultLocale))

  def fileNameMonthDayYear: String =
    currentDate.format(DateTimeFormatter.ofPattern("MM-dd-yyyy", DefaultLocale))

  def thisYear: String = currentDate.format(DateTimeFormatter.ofPattern("yyyy", DefaultLocale))

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

  def convertMonthToDate(month: String, year: Int): LocalDateTime = {
    val dateFormat = new SimpleDateFormat("MMMM yyyy")
    val monthDate  = dateFormat.parse(s"$month $year")

    monthDate.toInstant.atZone(DefaultTimezone).toLocalDateTime
  }

  def makeDate(year: Int, month: Int, day: Int): Date =
    Date.from(LocalDate.of(year, month, day).atStartOfDay(DefaultTimezone).toInstant)

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

  implicit class DateOps(val v: Date) extends AnyVal {
    def toLocalDate: LocalDate = v.toInstant.atZone(DefaultTimezone).toLocalDate
    def isYesterday: Boolean = {
      val localDate    = toLocalDate
      val day          = localDate.getDayOfYear
      val year         = localDate.getYear
      val yesterdayDay = yesterday
      day == yesterdayDay.getDayOfYear && year == yesterdayDay.getYear
    }
  }
}
