package com.mypetdefense.util

import java.time.format.DateTimeFormatter

import com.mypetdefense.AppConstants.DefaultLocale

object DateFormatters {
  lazy val `01`: DateTimeFormatter               = formatter("MM")
  lazy val `2021`: DateTimeFormatter             = formatter("yyyy")
  lazy val `01/21`: DateTimeFormatter            = formatter("MM/yy")
  lazy val `2021-1-1`: DateTimeFormatter         = formatter("y-M-d")
  lazy val `2021/01/01`: DateTimeFormatter       = formatter("yyyy/MM/dd")
  lazy val `2021-01-01`: DateTimeFormatter       = formatter("yyyy-MM-dd")
  lazy val `1/1/2021`: DateTimeFormatter         = formatter("M/d/y")
  lazy val `01/01/2021`: DateTimeFormatter       = formatter("MM/dd/yyyy")
  lazy val `01-01-2021`: DateTimeFormatter       = formatter("MM-dd-yyyy")
  lazy val `Jan 1`: DateTimeFormatter            = formatter("MMM d")
  lazy val `Jan 01`: DateTimeFormatter           = formatter("MMM dd")
  lazy val `Jan2021`: DateTimeFormatter          = formatter("MMMyyyy")
  lazy val `Jan 2021`: DateTimeFormatter         = formatter("MMM yyyy")
  lazy val `Jan 01, 2021`: DateTimeFormatter     = formatter("MMM dd, yyyy")
  lazy val `January 2021`: DateTimeFormatter     = formatter("MMMM yyyy")
  lazy val `January-2021`: DateTimeFormatter     = formatter("MMMM-yyyy")
  lazy val `January 1, 2021`: DateTimeFormatter  = formatter("MMMM d, yyyy")
  lazy val `January 01, 2021`: DateTimeFormatter = formatter("MMMM dd, yyyy")

  private def formatter(pattern: String): DateTimeFormatter =
    DateTimeFormatter.ofPattern(pattern, DefaultLocale)
}
