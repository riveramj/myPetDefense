package com.mypetdefense.util

import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter

import com.mypetdefense.AppConstants.DefaultLocale

import scala.language.implicitConversions

object DateFormatters {
  lazy val `01`               = twoFormatters("MM")
  lazy val `2021`             = twoFormatters("yyyy")
  lazy val `01/21`            = twoFormatters("MM/yy")
  lazy val `2021-1-1`         = twoFormatters("y-M-d")
  lazy val `2021-01-01`       = twoFormatters("yyyy-MM-dd")
  lazy val `1/1/2021`         = twoFormatters("M/d/y")
  lazy val `01/01/2021`       = twoFormatters("MM/dd/yyyy")
  lazy val `01-01-2021`       = twoFormatters("MM-dd-yyyy")
  lazy val `Jan`              = twoFormatters("MMM")
  lazy val `Jan 1`            = twoFormatters("MMM d")
  lazy val `Jan 01`           = twoFormatters("MMM dd")
  lazy val `Jan2021`          = twoFormatters("MMMyyyy")
  lazy val `Jan 2021`         = twoFormatters("MMM yyyy")
  lazy val `Jan-2021`         = twoFormatters("MMM-yyyy")
  lazy val `Jan 01, 2021`     = twoFormatters("MMM dd, yyyy")
  lazy val `January 2021`     = twoFormatters("MMMM yyyy")
  lazy val `January 1, 2021`  = twoFormatters("MMMM d, yyyy")
  lazy val `January 01, 2021` = twoFormatters("MMMM dd, yyyy")

  // TODO: remove when codebase is migrated to using DateTimeFormatters
  private def twoFormatters(pattern: String): (SimpleDateFormat, DateTimeFormatter) =
    (
      new SimpleDateFormat(pattern, DefaultLocale),
      DateTimeFormatter.ofPattern(pattern, DefaultLocale)
    )

  @deprecated("Migrate to DateTimeFormatter", since = "0.1-SNAPSHOT")
  implicit def asSimpleDateFormat(
      twoFormatters: (SimpleDateFormat, DateTimeFormatter)
  ): SimpleDateFormat =
    twoFormatters._1

  implicit def asDateTimeFormatter(
      twoFormatters: (SimpleDateFormat, DateTimeFormatter)
  ): DateTimeFormatter =
    twoFormatters._2
}
