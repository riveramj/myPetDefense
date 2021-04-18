package com.mypetdefense.model.domain.reports

import java.time.{LocalDate, Month}

import com.mypetdefense.model.Status
import com.mypetdefense.typeclasses.ToCsvStringConverter
import com.mypetdefense.util.csv.CSVHelper

case class RawSaleDataReport(
    year: Int,
    month: Month,
    mailedDate: LocalDate,
    customerUserId: Long,
    customerName: String,
    amountPaid: BigDecimal,
    salesAgentId: String,
    commision: BigDecimal,
    status: Status.Value
) {
  def toCsvRow: List[String] = {
    List(
      year.toString,
      month.toString,
      mailedDate.toString,
      customerUserId.toString,
      customerName,
      s"$$$amountPaid",
      salesAgentId,
      f"$$$commision%2.2f",
      status.toString
    )
  }

}

object RawSaleDataReport {
  implicit val rawSalesReportCsvConverter: ToCsvStringConverter[List[RawSaleDataReport]] =
    new ToCsvStringConverter[List[RawSaleDataReport]] {
      val headers =
        List(
          "Year",
          "Month",
          "Mailed Date",
          "Customer Id",
          "Customer Name",
          "Amount",
          "Call Agent Id",
          "Commision",
          "Customer Status"
        )
      override def toCsvString(
          input: List[RawSaleDataReport]
      ): String = {
        CSVHelper.toStringWithHeadersCsv(headers, input.map(_.toCsvRow))
      }
    }
}
