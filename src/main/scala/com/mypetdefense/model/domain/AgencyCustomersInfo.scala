package com.mypetdefense.model.domain

case class AgencyCustomersInfo(agencyName: String, customersInfo: List[CustomerInfo]) {
  def customersInfoToCSVRows: List[List[String]] = customersInfo.map(_.toCsvRow)
}
