package com.mypetdefense.model.domain.reports

import java.util.Date

import com.mypetdefense.model.AnimalType
import com.mypetdefense.model.domain.reports.AmazonOrderReport.dateFormat
import com.mypetdefense.typeclasses.ToCsvStringConverter
import com.mypetdefense.util.CSVHelper
import com.mypetdefense.util.DateFormatters._

case class AmazonOrderReport(
    name: String,
    address1: String,
    address2: String,
    address3: String,
    city: String,
    state: String,
    zip: String,
    animalType: AnimalType.Value,
    purchaseDate: Date,
    productName: String
) {
  def toCsvRow: List[String] = {
    val address =
      s"$address1 $address2 $address3 $city $state $zip".replaceAll(" {2}", "").trim()
    List(
      name,
      address,
      animalType.toString,
      dateFormat.format(purchaseDate),
      productName.replace(",", "")
    )
  }
}

object AmazonOrderReport {

  val dateFormat = `01/01/2021`

  implicit val amazonOrdersReportsToCSV: ToCsvStringConverter[List[AmazonOrderReport]] =
    new ToCsvStringConverter[List[AmazonOrderReport]] {
      val headers = List(
        "Name",
        "Address",
        "Animal Type",
        "Purchase Date",
        "Product"
      )
      override def toCsvString(input: List[AmazonOrderReport]): String = {
        CSVHelper.toStringWithHeadersCsv(headers, input.map(_.toCsvRow))
      }
    }

}
