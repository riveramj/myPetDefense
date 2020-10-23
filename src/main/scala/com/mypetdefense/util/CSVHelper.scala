package com.mypetdefense.util

import net.liftweb.http.InMemoryResponse

object CSVHelper {

  def generateCSV(csv: String, fileName: String): Some[InMemoryResponse] = {
    Some(
      InMemoryResponse(
        csv.getBytes("UTF-8"),
        List(
          "Content-Type"        -> "binary/octet-stream",
          "Content-Disposition" -> s"attachment; $fileName"
        ),
        Nil,
        200
      )
    )
  }

}
