package com.mypetdefense.model.domain.reports

import com.mypetdefense.typeclasses.ToCsvStringConverter
import com.mypetdefense.util.CSVHelper

case class AgencyCustomersReport(agencyName: String, customersInfo: List[CustomerDataReport]) {
  def customersInfoToCSV(headers: List[String]): String =
    CSVHelper.toStringWithHeadersCsv(headers, customersInfo.map(_.toCsvRow))
}

object AgencyCustomersReport {
  implicit val agencyToReportCsvConverter: ToCsvStringConverter[AgencyCustomersReport] =
    new ToCsvStringConverter[AgencyCustomersReport] {
      val headers = List("Name", "Status", "Shipment Count", "Signup Date", "Cancellation Date")
      override def toCsvString(input: AgencyCustomersReport): String = {
        CSVHelper.toStringWithHeadersCsv(headers, input.customersInfo.map(_.toCsvRow))
      }
    }
}
