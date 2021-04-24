package com.mypetdefense.util.csv

import net.liftweb.common._

final case class TrackingInfo(recipient: String, trackingNumber: String)

object TrackingUploadCSV extends GenericCSVParser[TrackingInfo] {
  val badChar: Set[Char] = "=\"".toSet

  override def convertRowToModel(
      fieldList: Array[String],
      lineCount: Int,
      headerIndex: Map[Columns.Value, Int]
  ): Box[TrackingInfo] = {
    import Columns._

    val recipient         = cellValue(Recipient, headerIndex, fieldList).openOr("")
    val rawTrackingNumber = cellValue(TrackingNumber, headerIndex, fieldList).openOr("")

    val trackingNumber = rawTrackingNumber.filterNot(badChar)

    Full(TrackingInfo(recipient, trackingNumber))
  }

  object Columns extends Columns {
    val Recipient: HeaderValue      = requiredValue("Recipient")
    val TrackingNumber: HeaderValue = requiredValue("Tracking #")
  }
}
