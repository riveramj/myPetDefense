package com.mypetdefense.model.domain.reports

import com.mypetdefense.typeclasses.ToCsvStringConverter
import com.mypetdefense.util.csv.CSVHelper.spacerRow

case class AgencyMtdSalesReport(
    totalUsers: Int,
    usersByCurrentMonth: Int,
    agentsSalesData: List[UsersMonthByAgentReport],
    allSalesForMonthData: String
) {
  def toCsvRow = {
    val topHeaders = List(
      "Sales Report",
      "New Sales by Qty"
    )
    val agentHeaders = List(
      "MTD Sales by Rep",
      "New MTD Sales by Qty"
    )
    val allSalesHeaders = List(
      "Rep ID",
      "Customer ID",
      "Date",
      "Value",
      "Estimated Commission"
    )
    val currentYearMTDSalesRow = List(f"Year To Date Totals,$totalUsers")
    val currentMonthSalesRow   = List(f"Month To Date Totals,$usersByCurrentMonth")
    val agentSalesData         = agentsSalesData.map(_.toCsvListList)

    {
      List(topHeaders) ++
        List(currentYearMTDSalesRow) ++
        List(currentMonthSalesRow) ++
        spacerRow ++
        List(agentHeaders) ++
        agentSalesData ++
        spacerRow ++
        List(allSalesHeaders)
    }.map(_.mkString(",")).mkString("\n") + "\n" + allSalesForMonthData
  }
}

case class UsersMonthByAgentReport(agentId: String, users: Int) {
  def toCsvListList: List[String] = {
    List(f"$agentId,$users")
  }
}

object AgencyMtdSalesReport {
  implicit val agencyMTDSalersReportToCsv: ToCsvStringConverter[AgencyMtdSalesReport] =
    new ToCsvStringConverter[AgencyMtdSalesReport] {
      val topHeaders = List(
        "Sales Report",
        "New Sales by Qty"
      )
      val agentHeaders = List(
        "MTD Sales by Rep",
        "New MTD Sales by Qty"
      )
      val allSalesHeaders = List(
        "Rep ID",
        "Customer ID",
        "Date",
        "Value",
        "Estimated Commission"
      )
      override def toCsvString(input: AgencyMtdSalesReport): String = {
        val currentYearMTDSalesRow = List(f"Year To Date Totals,${input.totalUsers}")
        val currentMonthSalesRow   = List(f"Month To Date Totals,${input.usersByCurrentMonth}")
        val agentSalesData         = input.agentsSalesData.map(_.toCsvListList)

        {
          List(topHeaders) ++
            List(currentYearMTDSalesRow) ++
            List(currentMonthSalesRow) ++
            spacerRow ++
            List(agentHeaders) ++
            agentSalesData ++
            spacerRow ++
            List(allSalesHeaders)
        }.map(_.mkString(",")).mkString("\n") + "\n" + input.allSalesForMonthData
      }
    }
}
