package com.mypetdefense.model.domain.reports

import java.time.Month

import com.mypetdefense.model.Status
import com.mypetdefense.typeclasses.ToCsvStringConverter
import com.mypetdefense.util.csv.CSVHelper.spacerRow
import net.liftweb.common.Box

case class CancellationDataReport(
    customerStatusTotals: List[CustomerStatusTotalsReport],
    currentYearCancelSales: CurrentYearSalesReport,
    currentMonthCancelSales: CurrentMonthCancelSalesReport,
    allCancellationRows: List[AllCancellationReport]
) {
  def allCancellationRowsToCsvInput: List[List[String]] = {
    allCancellationRows.map { r =>
      List(
        r.customerId.toString,
        r.firstShipmentDate.toString,
        r.lastShipmentDate.toString,
        r.subscriptionStatus.toString,
        r.shipmentCount.toString,
        s"$$${r.totalGrossSales}",
        f"$$${r.totalCommission}%2.2f"
      )
    }
  }

  def customerStatusTotalsAsCsv: List[List[String]] = {
    customerStatusTotals.map { customer =>
      s"${customer.status.headOption.getOrElse("")} Users,${customer.size}"
    }.sorted.map(_ :: "," :: Nil)
  }
}

case class CustomerStatusTotalsReport(status: Box[Status.Value], size: Int)

case class CurrentYearSalesReport(
    currentYearSubscriptionCancels: Int,
    averageShipmentsPerCancelByYear: BigDecimal,
    currentYearCancelTotal: BigDecimal,
    yearCancelCommisionAmount: BigDecimal
) {
  def toCsvRow =
    List(
      f"Year To Date Totals,$currentYearSubscriptionCancels,$averageShipmentsPerCancelByYear%3.2f,$$$currentYearCancelTotal,$$$yearCancelCommisionAmount%3.2f"
    )

}

case class CurrentMonthCancelSalesReport(
    currentMonthSubscriptionCancels: Int,
    averageShipmentsPerCancelByMonth: BigDecimal,
    currentMonthCancelTotal: BigDecimal,
    monthCancelCommisionAmount: BigDecimal
) {
  def toCsvRow: List[String] = List(
    f"Month To Date Totals,${currentMonthSubscriptionCancels},$averageShipmentsPerCancelByMonth%3.2f,$$$currentMonthCancelTotal,$$$monthCancelCommisionAmount%3.2f"
  )
}

case class AllCancellationReport(
    customerId: Long,
    firstShipmentDate: Month,
    lastShipmentDate: Month,
    subscriptionStatus: Status.Value,
    shipmentCount: Int,
    totalGrossSales: BigDecimal,
    totalCommission: BigDecimal
)

object CancellationDataReport {
  implicit val toCsvConverter: ToCsvStringConverter[CancellationDataReport] =
    new ToCsvStringConverter[CancellationDataReport] {
      val topHeaders = List(
        "Cancellation Report",
        "YTD Cancels by Qty",
        "Avg Shipments",
        "Gross YTD Cancels by $",
        "Estimated Commission"
      )
      val headers = List(
        "Customer Id",
        "Start Month",
        "End Month",
        "Customer Status",
        "Shipment Count",
        "Total Gross Sales",
        "Total Commission"
      )
      override def toCsvString(input: CancellationDataReport): String = {
        input.customerStatusTotalsAsCsv ++
          spacerRow ++
          List(topHeaders) ++
          List(input.currentYearCancelSales.toCsvRow) ++
          List(input.currentMonthCancelSales.toCsvRow) ++
          spacerRow ++
          List(headers) ++
          input.allCancellationRowsToCsvInput
      }.map(_.mkString(",")).mkString("\n")
    }
}
