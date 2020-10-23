package com.mypetdefense.util

import net.liftweb.http.InMemoryResponse

object CSVHelper {

  def generateCSV(csv: String, fileName: String): Some[InMemoryResponse] = {
    val file = "filename=\"" + fileName + "\""
    Some(
      InMemoryResponse(
        csv.getBytes("UTF-8"),
        List(
          "Content-Type"        -> "binary/octet-stream",
          "Content-Disposition" -> s"attachment; $file"
        ),
        Nil,
        200
      )
    )
  }

}
