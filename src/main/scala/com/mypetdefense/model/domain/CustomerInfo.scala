package com.mypetdefense.model.domain

import com.mypetdefense.model.Status

case class CustomerInfo(
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
