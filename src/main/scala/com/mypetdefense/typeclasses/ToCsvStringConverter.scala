package com.mypetdefense.typeclasses

trait ToCsvStringConverter[T] {
  def toCsvString(input: T): String
}

object ToCsvStringConverter {
  implicit class ToReportCsvConverterOps[T](val v: T) extends AnyVal {
    def toCsvString(implicit converter: ToCsvStringConverter[T]): String = converter.toCsvString(v)
  }
}
