package com.mypetdefense.util

import java.time._

import com.mypetdefense.AppConstants.DefaultTimezone
import com.mypetdefense.util.DateFormatters._
import net.liftweb.util.Helpers.tryo

object DateHelper {
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

  def fileNameYearMonth: String    = currentDate.format(`Jan2021`)
  def fileNameMonthDayYear: String = currentDate.format(`01-01-2021`)
  def thisYear: String             = currentDate.format(`2021`)

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

  def convertMonthToDate(month: String, year: Int): LocalDateTime =
    YearMonth.parse(s"$month $year", `January 2021`).atDay(1).atStartOfDay()

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

  implicit class ZonedDateTimeOps(val self: ZonedDateTime) extends AnyVal {
    def isYesterday: Boolean = {
      val localDate    = self.toLocalDate
      val day          = localDate.getDayOfYear
      val year         = localDate.getYear
      val yesterdayDay = yesterday
      day == yesterdayDay.getDayOfYear && year == yesterdayDay.getYear
    }
  }
}
