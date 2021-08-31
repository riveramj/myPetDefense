package com.mypetdefense.model.domain.reports

import com.mypetdefense.typeclasses.ToCsvStringConverter
import com.mypetdefense.util.csv.CSVHelper.spacerRow

case class AgencyMonthSalesReport(
    year: Int,
    month: String,
    netNewUsersMonth: Int,
    totalCancelledSubscriptions: Int,
    paidMonthShipments: Int,
    paidMonthPetsShippedCount: Int,
    paidMonthGrossSales: BigDecimal,
    paidMonthCommission: BigDecimal
) {
  def toCsvRow: String = {
    {
      List(List("Time Period", s"$month $year")) ++
        spacerRow ++
        List(List("Net New Users", netNewUsersMonth)) ++
        List(List("Total Cancellations", totalCancelledSubscriptions)) ++
        List(List("Paid Shipments", paidMonthShipments)) ++
        List(List("Paid Pets", paidMonthPetsShippedCount)) ++
        List(List("Gross Sales", f"$$$paidMonthGrossSales%2.2f")) ++
        List(List("Estimated Commission", f"$$$paidMonthCommission%2.2f"))
    }.map(_.mkString(",")).mkString("\n")
  }
}

object AgencyMonthSalesReport {
  implicit val agencyMonthSalesCsvConverter: ToCsvStringConverter[AgencyMonthSalesReport] =
    ToCsvStringConverter.fromFunction(_.toCsvRow)
}
