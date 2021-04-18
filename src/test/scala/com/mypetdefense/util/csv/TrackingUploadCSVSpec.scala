package com.mypetdefense.util.csv

import net.liftweb.common.Full

class TrackingUploadCSVSpec extends GenericCSVParserSpec[TrackingInfo] {

  override val parser = TrackingUploadCSV

  it should "properly parse TrackingUploadCSV" in {
    val csv =
      """Recipient,Tracking #
        |"Some recipient",001""".stripMargin

    csv mustBeParsedTo
      Full(
        List(
          TrackingInfo("Some recipient", "001")
        )
      )
  }
}
