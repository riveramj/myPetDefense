package com.mypetdefense.model.domain.reports

import java.time.Month

import com.mypetdefense.typeclasses.ToCsvStringConverter
import com.mypetdefense.util.CSVHelper

case class CancelsInMonthReport(month: Month, cancels: Int) {
  def toCsvRow: List[String] = {
    List(month.toString, cancels.toString)
  }
}

object CancelsInMonthReport {
  implicit val agencyToReportCsvConverter: ToCsvStringConverter[List[CancelsInMonthReport]] =
    new ToCsvStringConverter[List[CancelsInMonthReport]] {
      val headers = List("Month", "Cancel Count")
      override def toCsvString(input: List[CancelsInMonthReport]): String = {
        CSVHelper.toStringWithHeadersCsv(headers, input.map(_.toCsvRow))
      }
    }
}
