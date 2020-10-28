package com.mypetdefense.util

import com.mypetdefense.typeclasses.ToCsvStringConverter._
import com.mypetdefense.typeclasses.ToCsvStringConverter
import net.liftweb.http.InMemoryResponse

object CSVHelper {

  val spacerRow = List(List(","))

  type CSVInput = List[List[String]]

  def generateCSV(csv: String, fileName: String): InMemoryResponse = {
    val file = "filename=\"" + fileName + "\""
    InMemoryResponse(
      csv.getBytes("UTF-8"),
      List(
        "Content-Type"        -> "binary/octet-stream",
        "Content-Disposition" -> s"attachment; $file"
      ),
      Nil,
      200
    )
  }

  def toStringWithHeadersCsv(headers: List[String], data: CSVInput): String =
    (List(headers) ++ data).map(_.mkString(",")).mkString("\n")

  def inMemoryCsv[T: ToCsvStringConverter](fileName: String, input: T): InMemoryResponse = {
    generateCSV(input.toCsvString, fileName)
  }

}
