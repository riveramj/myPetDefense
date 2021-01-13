package com.mypetdefense.util

import java.text.SimpleDateFormat
import java.time._
import java.time.format.DateTimeFormatter
import java.util.Date

import com.mypetdefense.AppConstants.{DefaultLocale, DefaultTimezone}
import net.liftweb.util.Helpers.tryo

object DateHelper {
  val dateFormat = new SimpleDateFormat("MM/dd/yyyy")

  def currentDate: LocalDateTime = LocalDateTime.now()
  def now: LocalDate             = LocalDate.now(DefaultTimezone)

  def nowAtStartOfDay: ZonedDateTime = now.atStartOfDay(DefaultTimezone)
  def yesterday: ZonedDateTime       = nowAtStartOfDay.minusDays(1)
  def yesterdayStart: ZonedDateTime  = nowAtStartOfDay.minusDays(1)
  def yesterdayEnd: ZonedDateTime    = nowAtStartOfDay
  def threeDaysAgo: ZonedDateTime    = now.minusDays(3).atStartOfDay(DefaultTimezone)
  def sixtyDaysAgo: ZonedDateTime    = now.minusDays(60).atStartOfDay(DefaultTimezone)
  def monthDayOne: ZonedDateTime     = now.withDayOfMonth(1).atStartOfDay(DefaultTimezone)
  def monthDayOneLastMonth: ZonedDateTime =
    now.withDayOfMonth(1).atStartOfDay(DefaultTimezone).minusMonths(1)
  def currentDayLastMonthEnd: ZonedDateTime = nowAtStartOfDay.withDayOfMonth(1)
  def monthDayOneLastYear: ZonedDateTime =
    now.withDayOfMonth(1).atStartOfDay(DefaultTimezone).minusYears(1)
  def currentDayLastYearEnd: ZonedDateTime = nowAtStartOfDay.plusDays(1).minusYears(1)
  def tomorrowStart: ZonedDateTime         = nowAtStartOfDay.plusDays(1)
  def beginningNextMonth: ZonedDateTime =
    YearMonth.now().atEndOfMonth().atStartOfDay(DefaultTimezone).plusDays(1)
  def yearDayOne: ZonedDateTime = now.withDayOfYear(1).atStartOfDay(DefaultTimezone)
  def yearDayOneLastYear: ZonedDateTime =
    now.withDayOfYear(1).atStartOfDay(DefaultTimezone).minusYears(1)
  def todayLastMonth: ZonedDateTime    = nowAtStartOfDay.minusMonths(1)
  def todayLastYear: ZonedDateTime     = nowAtStartOfDay.minusYears(1)
  def todayLastYearEnd: ZonedDateTime  = nowAtStartOfDay.minusYears(1).plusDays(1)
  def todayLastMonthEnd: ZonedDateTime = nowAtStartOfDay.minusMonths(1).plusDays(1)

  def fileNameYearMonth: String =
    currentDate.format(DateTimeFormatter.ofPattern("MMMyyyy", DefaultLocale))
  def fileNameMonthDayYear: String =
    currentDate.format(DateTimeFormatter.ofPattern("MM-dd-yyyy", DefaultLocale))

  def thisYear: String = currentDate.format(DateTimeFormatter.ofPattern("yyyy", DefaultLocale))

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

  implicit class DateOps(val self: Date) extends AnyVal {
    def toZonedDateTime: ZonedDateTime = self.toInstant.atZone(DefaultTimezone)
  }

  implicit class ZonedDateTimeOps(val self: ZonedDateTime) extends AnyVal {
    def toDate: Date = Date.from(self.toInstant)
    def isYesterday: Boolean = {
      val localDate    = self.toLocalDate
      val day          = localDate.getDayOfYear
      val year         = localDate.getYear
      val yesterdayDay = yesterday
      day == yesterdayDay.getDayOfYear && year == yesterdayDay.getYear
    }
  }
}
