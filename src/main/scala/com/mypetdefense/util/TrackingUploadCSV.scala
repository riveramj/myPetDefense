package com.mypetdefense.util

import scala.collection.JavaConverters._
import scala.util.matching.Regex
import java.io.StringReader
import java.util.regex.Pattern

import net.liftweb.http.S
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.mapper.By
import au.com.bytecode.opencsv.{CSVParser, CSVReader}

import xml.Text
import com.mypetdefense.model._
import java.nio.charset.StandardCharsets

import scala.language.implicitConversions
import scala.language.postfixOps
import java.text.SimpleDateFormat
import java.util.Date

import com.mypetdefense.util.TrackingUploadCSV.Columns

case class TrackingInfo(recipient: String, trackingNumber: String)

case class TrackingInfoList(list: List[TrackingInfo])

object TrackingUploadCSV extends Loggable {
  val badChar: Set[Char] = "=\"".toSet

  def parse(source: Array[Byte]): Box[TrackingInfoList] =
    parse(new String(source, StandardCharsets.UTF_8))

  object Columns extends Enumeration {
    val Recipient: HeaderValue =
      HeaderValue(name = "Recipient", default = Failure("missing required Recipient"))
    val TrackingNumber: HeaderValue =
      HeaderValue(name = "Tracking #", default = Failure("missing required Tracking #"))

    case class HeaderValue(
        name: String,
        matcher: Box[Regex] = Empty,
        parser: String => Box[String] = (rawCell: String) => Full(rawCell.trim).filter(_.nonEmpty),
        default: Box[String] = Empty
    ) extends Val(name) {
      val nameMatcher: Regex = matcher openOr ("""(?i)^\s*""" + Pattern.quote(name) + """\s*$""").r
      def matches(candidate: String): Boolean = {
        nameMatcher.unapplySeq(candidate).isDefined
      }
      def required: Boolean = {
        default match {
          case Failure(_, _, _) => true
          case _                => false
        }
      }
    }

    def requiredColumns: Columns.ValueSet = {
      values.filter(_.required)
    }

    def requiredColumnsCount: Int = {
      requiredColumns.size
    }

    def missingRequiredHeaders(headerIndex: Map[Columns.Value, Int]): Columns.ValueSet = {
      requiredColumns.filter(headerIndex.get(_).isEmpty)
    }

    def cellValue(
        column: HeaderValue,
        headerIndex: Map[Value, Int],
        row: Array[String]
    ): Box[String] = {
      Box {
        for {
          index <- headerIndex get column
          cell  <- row.lift(index)
          value <- column.parser(cell)
        } yield {
          value
        }
      } or column.default
    }

    implicit def valueToHeaderValue(v: Value): HeaderValue = v.asInstanceOf[HeaderValue]
  }

  def parse(source: String): Box[TrackingInfoList] = {
    val trackingInfoList: List[Box[TrackingInfo]] = {
      val emptyMsg      = "File was empty"
      val lines         = source split ("\r\n|[\r\n]")
      val filteredLines = lines map (_ trim) filterNot (_ isEmpty)
      // User uploaded Empty File
      if (filteredLines isEmpty) {
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
            allRows.take(1)(0).zipWithIndex.flatMap {
              case (header, i) =>
                Columns.values.find(_.matches(header.trim)).map(_ -> i)
            }

          val lastRequiredColumn =
            headers.filter(_._1.required).last._2

          val headerIndex = headers toMap

          val missingRequiredColumns = Columns.missingRequiredHeaders(headerIndex)

          if (missingRequiredColumns.size > 0) {
            Failure(
              ("Required columns missing: %s") format missingRequiredColumns
                .map(_.name)
                .mkString(", ")
            ) :: Nil
          } else {
            val rowsToRead = allRows.drop(1)
            if (rowsToRead.isEmpty) {
              Failure(emptyMsg) :: Nil // File with only header
            } else {
              (
                rowsToRead.zipWithIndex map {
                  case (fieldList, lineCount) =>
                    logger.debug(
                      "line:%s Contents: %s" format (lineCount + 1, Text(fieldList.mkString(", ")))
                    )
                    parseLine(fieldList, lineCount + 1, headerIndex, lastRequiredColumn)
                }
              ).toList
            }
          }
        } catch {
          case e: Throwable => {
            val message = "Cannot parse file"
            logger.error(message, e)
            Failure(message) :: Nil
          }
        }
      }
    }

    val errors = trackingInfoList filter (_.isEmpty) filter (_ != Empty)

    if (errors.isEmpty) {
      val successfulTrackingInfo = trackingInfoList collect {
        case Full(trackingInfo) => trackingInfo
      }
      Full(TrackingInfoList(successfulTrackingInfo.toList))
    } else {
      val errorMessages: List[String] = errors
        .map(_ match {
          case Failure(msg, _, _) => msg
          case _                  => ""
        })
        .filter(_.nonEmpty)
      Empty ~> errorMessages
    }
  }

  private def failLine(msg: String, line: Int) = {
    Failure("%s [line %d]" format (msg, line))
  }

  def parseLine(
      fieldList: Array[String],
      lineCount: Int,
      headerIndex: Map[Columns.Value, Int],
      lastRequiredColumn: Int
  ): Box[TrackingInfo] = {
    if (fieldList.isEmpty || fieldList.foldLeft("")(_ + _).trim.isEmpty) {
      Empty
    } else {
      fieldList.length match {
        case 0                                      => Empty
        case length if length <= lastRequiredColumn => failLine(S.?("Not enough fields"), lineCount)
        case _                                      => toTrackingInfo(fieldList, lineCount, headerIndex)
      }
    }
  }

  def toTrackingInfo(
      fieldList: Array[String],
      lineCount: Int,
      headerIndex: Map[Columns.Value, Int]
  ): Box[TrackingInfo] = {
    val recipient = Columns.cellValue(Columns.Recipient, headerIndex, fieldList).openOr("")
    val rawTrackingNumber =
      Columns.cellValue(Columns.TrackingNumber, headerIndex, fieldList).openOr("")

    val trackingNumber = rawTrackingNumber.filterNot(badChar)

    Full(TrackingInfo(recipient, trackingNumber))
  }
}
