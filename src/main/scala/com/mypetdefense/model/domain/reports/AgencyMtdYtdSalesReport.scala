package com.mypetdefense.model.domain.reports

import com.mypetdefense.typeclasses.ToCsvStringConverter
import com.mypetdefense.util.csv.CSVHelper.spacerRow

case class AgencyMtdYtdSalesReport(
    mtdNewCustomers: NewCustomersMtdYtdSalesReport,
    ytdNewCustomers: NewCustomersMtdYtdSalesReport,
    mtdPayingCustomers: PayingCustomersMtdYtdSalesReport,
    ytdPayingCustomers: PayingCustomersMtdYtdSalesReport,
    paidCustomersByAgent: List[PaidCustomersByAgentMtdYtdSalesReport],
    newCustomersByAgent: List[NewCustomersByAgent]
)

case class NewCustomersMtdYtdSalesReport(
    newUsers: Int,
    newUsersSubscriptionCancelled: Int,
    newUsersSubscriptionActive: Int,
    newUsersShipments: Int,
    newUsersShippedPetCount: Int
) {
  def toCsvRow(header: String) =
    List(
      s"$header,$newUsers,$newUsersSubscriptionCancelled,$newUsersSubscriptionActive,$newUsersShipments,$newUsersShippedPetCount"
    )
}

case class PayingCustomersMtdYtdSalesReport(
    allPayingActiveSubscriptions: Int,
    paidSubscriptionCancelled: Int,
    paidShipments: Int,
    paidPetsShippedCount: Int,
    paidGrossSales: BigDecimal,
    paidCommission: BigDecimal
) {
  def toCsvRow(header: String) =
    List(
      f"$header,$allPayingActiveSubscriptions,$paidSubscriptionCancelled,$paidShipments,$paidPetsShippedCount,$$$paidGrossSales%2.2f,$$$paidCommission%2.2f"
    )
}

case class PaidCustomersByAgentMtdYtdSalesReport(
    agentId: String,
    activePayingAgentCustomer: Int,
    cancelledPayingAgentCustomer: Int,
    shipments: Int,
    shipmentsPetCount: Int,
    shipmentsTotal: BigDecimal,
    commisionTotal: BigDecimal
) {
  def toCsvRow: List[String] =
    List(
      f"$agentId,$activePayingAgentCustomer,$cancelledPayingAgentCustomer,$shipments,$shipmentsPetCount,$$$shipmentsTotal%2.2f,$$$commisionTotal%2.2f"
    )
}

case class NewCustomersByAgent(
    agentId: String,
    activeNewAgentCustomer: Int,
    cancelledNewAgentCustomer: Int,
    shipments: Int,
    shipmentsPetCount: Int
) {
  def toCsvRow = List(
    f"$agentId,$activeNewAgentCustomer,$cancelledNewAgentCustomer,$shipments,$shipmentsPetCount"
  )
}

object AgencyMtdYtdSalesReport {
  implicit val agencyMtdYtdSalesReportToCsvStringConverter
      : ToCsvStringConverter[AgencyMtdYtdSalesReport] =
    new ToCsvStringConverter[AgencyMtdYtdSalesReport] {
      val newCustomerHeaders = List(
        "",
        "New Customers",
        "Cancellations",
        "Net New Customers",
        "New Customer Shipments",
        "New Pets Shipped"
      )

      val payingCustomerHeaders = List(
        "",
        "Paying Customers",
        "Cancellations",
        "Paid Shipments",
        "Paid Pets Shipped",
        "Gross Sales",
        "Estimated Comission"
      )

      val agentNewCustomerHeaders = List(
        "MTD New Sales by Agent",
        "Net New Customers",
        "Cancellations",
        "Total Shipments",
        "Total Pets"
      )

      val agentPayingCustomerHeaders = List(
        "MTD Paying Sales by Agent",
        "Paying Customers",
        "Cancellations",
        "Paid Shipments",
        "Paid Pets Shipped",
        "Gross Sales",
        "Estimated Commission"
      )
      override def toCsvString(input: AgencyMtdYtdSalesReport): String = {
        List(newCustomerHeaders) ++
          List(input.mtdNewCustomers.toCsvRow("Month to Date")) ++
          List(input.ytdNewCustomers.toCsvRow("Year to Date")) ++
          spacerRow ++
          List(payingCustomerHeaders) ++
          List(input.mtdPayingCustomers.toCsvRow("Month to Date")) ++
          List(input.ytdPayingCustomers.toCsvRow("Year to Date")) ++
          spacerRow ++
          List(agentPayingCustomerHeaders) ++
          input.paidCustomersByAgent.map(_.toCsvRow) ++
          spacerRow ++
          List(agentNewCustomerHeaders) ++
          input.newCustomersByAgent.map(_.toCsvRow)
      }.map(_.mkString(",")).mkString("\n")
    }
}
