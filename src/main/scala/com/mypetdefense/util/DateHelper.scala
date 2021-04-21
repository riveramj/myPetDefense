package com.mypetdefense.util

import net.liftweb.common.Box
import net.liftweb.util.Helpers.tryo

import java.text.SimpleDateFormat
import java.time._
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.temporal.ChronoUnit.MILLIS
import java.util.{Date, Locale}

object DateHelper {
  val dateFormat     = new SimpleDateFormat("MM/dd/yyyy")
  val dateFormatDashes     = new SimpleDateFormat("MM-dd-yyyy")
  val zoneId: ZoneId = ZoneId.of("America/New_York")

  def datePlusDays(date: Date, days: Int): Date = {
    Date.from(date
      .toInstant
      .atZone(zoneId)
      .toLocalDate
      .plusDays(days)
      .atStartOfDay(zoneId)
      .toInstant
    )
  }
  def nowMillisAsInstant(): Instant = Instant.now().truncatedTo(MILLIS)

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

  def threeDaysAgo: Date = Date.from(now.minusDays(3).atStartOfDay(zoneId).toInstant)

  def sixtyDaysAgo: Date = Date.from(now.minusDays(60).atStartOfDay(zoneId).toInstant)

  def monthDayOne: Date = Date.from(now.withDayOfMonth(1).atStartOfDay(zoneId).toInstant)

  def monthDayOneLastMonth: Date =
    Date.from(now.withDayOfMonth(1).atStartOfDay(zoneId).minusMonths(1).toInstant)

  def currentDayLastMonthEnd: Date = Date.from(nowAtStartOfDay.withDayOfMonth(1).toInstant)

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

  def getMonthDiff(dateOne: Date, dateTwo: Box[Date]): Long = {
    val startDate = dateOne.toLocalDate
    val endDate =
      if (dateTwo.contains(null))
        now
      else
        dateTwo.openOr(nowDate).toLocalDate

    ChronoUnit.MONTHS.between(startDate, endDate)
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

    monthDate.toInstant.atZone(ZoneId.systemDefault()).toLocalDateTime
  }

  def makeDate(year: Int, month: Int, day: Int): Date =
    Date.from(LocalDate.of(year, month, day).atStartOfDay(zoneId).toInstant)

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
    def toLocalDate: LocalDate = v.toInstant.atZone(zoneId).toLocalDate
    def isYesterday: Boolean = {
      val localDate    = toLocalDate
      val day          = localDate.getDayOfYear
      val year         = localDate.getYear
      val yesterdayDay = yesterday
      day == yesterdayDay.getDayOfYear && year == yesterdayDay.getYear
    }
  }
}
