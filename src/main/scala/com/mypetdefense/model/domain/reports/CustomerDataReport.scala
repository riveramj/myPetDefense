package com.mypetdefense.model.domain.reports

import com.mypetdefense.model.Status

case class CustomerDataReport(
    name: String,
    subscriptionStatus: Status.Value,
    shipmentsSize: Int,
    formattedStartDate: String,
    formattedCancelDate: String
) {
  def toCsvRow: List[String] = {
    val statusString = if (subscriptionStatus == Status.Active) "Active" else "Inactive"
    List(name, statusString, shipmentsSize.toString, formattedStartDate, formattedCancelDate)
  }
}
