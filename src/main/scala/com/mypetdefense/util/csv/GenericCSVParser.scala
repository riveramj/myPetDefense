package com.mypetdefense.util.csv

import java.io.StringReader
import java.nio.charset.StandardCharsets
import java.util.regex.Pattern

import au.com.bytecode.opencsv.{CSVParser, CSVReader}
import net.liftweb.common._
import net.liftweb.http.S

import scala.collection.JavaConverters._
import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.xml.Text

abstract class GenericCSVParser[R] extends Loggable {
  type Columns = GenericCSVParser.Columns
  val Columns: Columns
  import Columns._

  def parse(source: Array[Byte]): Box[List[R]] =
    parse(new String(source, StandardCharsets.UTF_8))

  def parse(source: String): Box[List[R]] = {
    val rows: List[Box[R]] = {
      val emptyMsg      = "File was empty"
      val lines         = source.split("\r\n|[\r\n]")
      val filteredLines = lines.map(_.trim).filter(_.nonEmpty)
      // User uploaded Empty File
      if (filteredLines.isEmpty) {
        Failure(emptyMsg) :: Nil
      } else {
        try {
          val reader = new CSVReader(
            new StringReader(source),
            CSVParser.DEFAULT_SEPARATOR,
            CSVParser.DEFAULT_QUOTE_CHARACTER,
            0
          )
          val allRows = reader.readAll().asScala

          val headers =
            allRows.take(1).head.zipWithIndex.flatMap {
              case (header, i) =>
                Columns.values.find(_.matches(header.trim)).map(_ -> i)
            }

          val lastRequiredColumn =
            headers.filter(_._1.required).last._2

          val headerIndex = headers.toMap

          val missingRequiredColumns = Columns.missingRequiredHeaders(headerIndex)

          if (missingRequiredColumns.nonEmpty)
            Failure(
              "Required columns missing: " + missingRequiredColumns
                .map(_.name)
                .mkString(", ")
            ) :: Nil
          else {
            val rowsToRead = allRows.drop(1)
            if (rowsToRead.isEmpty) Failure(emptyMsg) :: Nil // File with only header
            else
              rowsToRead.zipWithIndex.map {
                case (fieldList, lineCount) =>
                  logger.debug(
                    s"line:${lineCount + 1} Contents: ${Text(fieldList.mkString(", "))}"
                  )
                  parseLine(fieldList, lineCount + 1, headerIndex, lastRequiredColumn)
              }.toList
          }
        } catch {
          case e: Throwable =>
            val message = "Cannot parse file"
            logger.error(message, e)
            Failure(message) :: Nil
        }
      }
    }

    val errors = rows.filter(_.isEmpty).filter(_ != Empty)

    if (errors.isEmpty) {
      val successfulRows = rows collect { case Full(row) => row }
      Full(successfulRows)
    } else {
      val errorMessages: List[String] = errors.map {
        case Failure(msg, _, _) => msg
        case _                  => ""
      }.filter(_.nonEmpty)
      Empty ~> errorMessages
    }
  }

  def parseLine(
      fieldList: Array[String],
      lineCount: Int,
      headerIndex: Map[Columns.Value, Int],
      lastRequiredColumn: Int
  ): Box[R] = {
    if (fieldList.isEmpty || fieldList.foldLeft("")(_ + _).trim.isEmpty)
      Empty
    else
      fieldList.length match {
        case 0                                      => Empty
        case length if length <= lastRequiredColumn => failLine(S ? "Not enough fields", lineCount)
        case _                                      => convertRowToModel(fieldList, lineCount, headerIndex)
      }
  }

  def convertRowToModel(
      fieldList: Array[String],
      lineCount: Int,
      headerIndex: Map[Columns.Value, Int]
  ): Box[R]

  private def failLine(msg: String, line: Int): Failure = Failure(s"$msg [line $line]")
}

object GenericCSVParser {
  abstract class Columns extends Enumeration {
    def requiredColumnsCount: Int =
      requiredColumns.size

    def requiredColumns: ValueSet =
      values.filter(_.required)

    def missingRequiredHeaders(headerIndex: Map[Value, Int]): ValueSet =
      requiredColumns.filter(!headerIndex.contains(_))

    def cellValue(
        column: HeaderValue,
        headerIndex: Map[Value, Int],
        row: Array[String]
    ): Box[String] =
      Box {
        for {
          index <- headerIndex.get(column)
          cell  <- row.lift(index)
          value <- column.parser(cell)
        } yield value
      } or column.default

    def cellInt(column: HeaderValue, headerIndex: Map[Value, Int], row: Array[String]): Box[Int] =
      cellValue(column, headerIndex, row).flatMap(i => Box.tryo(i.toInt))

    def cellDouble(
        column: HeaderValue,
        headerIndex: Map[Value, Int],
        row: Array[String]
    ): Box[Double] =
      cellValue(column, headerIndex, row).flatMap(i => Box.tryo(i.toDouble))

    def cellBoolean(
        column: HeaderValue,
        headerIndex: Map[Value, Int],
        row: Array[String]
    ): Boolean =
      cellValue(column, headerIndex, row).map(_.toLowerCase) match {
        case Full("1") | Full("y") | Full("yes") => true
        case _                                   => false
      }

    final case class HeaderValue(
        name: String,
        matcher: Box[Regex] = Empty,
        parser: String => Box[String] = (rawCell: String) => Full(rawCell.trim).filter(_.nonEmpty),
        default: Box[String] = Empty
    ) extends Val(name) {
      val nameMatcher: Regex =
        matcher openOr ("""(?i)^\s*""" + Pattern.quote(name) + """\s*$""").r

      def matches(candidate: String): Boolean =
        nameMatcher.unapplySeq(candidate).isDefined

      def required: Boolean = default match {
        case Failure(_, _, _) => true
        case _                => false
      }
    }

    def requiredValue(name: String): HeaderValue =
      HeaderValue(name, default = Failure(s"missing required $name"))

    implicit def valueToHeaderValue(v: Value): HeaderValue =
      v.asInstanceOf[HeaderValue]
  }
}
