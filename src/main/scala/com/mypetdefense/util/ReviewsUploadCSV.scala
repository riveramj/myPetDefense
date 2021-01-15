package com.mypetdefense.util

import java.io.StringReader
import java.nio.charset.StandardCharsets
import java.util.regex.Pattern

import au.com.bytecode.opencsv.{CSVParser, CSVReader}
import com.mypetdefense.model._
import com.mypetdefense.util.DateFormatters._
import com.mypetdefense.util.DateHelper._
import net.liftweb.common._
import net.liftweb.http.S
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._

import scala.collection.JavaConverters._
import scala.language.{implicitConversions, postfixOps}
import scala.util.matching.Regex
import scala.xml.Text

case class ReviewsList(list: List[Review])

object ReviewsUploadCSV extends Loggable {
  def parse(source: Array[Byte]): Box[ReviewsList] =
    parse(new String(source, StandardCharsets.UTF_8))

  def parse(source: String): Box[ReviewsList] = {
    val reviewsList: List[Box[Review]] = {
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

    val errors = reviewsList filter (_.isEmpty) filter (_ != Empty)

    if (errors.isEmpty) {
      val successReviews = reviewsList collect {
        case Full(review) => review
      }
      Full(ReviewsList(successReviews.toList))
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

  def parseLine(
      fieldList: Array[String],
      lineCount: Int,
      headerIndex: Map[Columns.Value, Int],
      lastRequiredColumn: Int
  ): Box[Review] = {
    if (fieldList.isEmpty || fieldList.foldLeft("")(_ + _).trim.isEmpty) {
      Empty
    } else {
      fieldList.length match {
        case 0                                      => Empty
        case length if length <= lastRequiredColumn => failLine(S.?("Not enough fields"), lineCount)
        case _                                      => toReview(fieldList, lineCount, headerIndex)
      }
    }
  }

  private def failLine(msg: String, line: Int) = {
    Failure("%s [line %d]" format (msg, line))
  }

  def toReview(
      fieldList: Array[String],
      lineCount: Int,
      headerIndex: Map[Columns.Value, Int]
  ): Box[Review] = {
    val title       = Columns.cellValue(Columns.Title, headerIndex, fieldList).openOr("")
    val body        = Columns.cellValue(Columns.Body, headerIndex, fieldList).openOr("")
    val author      = Columns.cellValue(Columns.Author, headerIndex, fieldList).openOr("")
    val productName = Columns.cellValue(Columns.Product, headerIndex, fieldList).openOr("")

    val product = (productName match {
      case "ZoGuard Plus for Dogs" => FleaTick.findAll(By(FleaTick.name, "ZoGuard Plus for Dogs"))
      case "ZoGuard Plus for Cats" => FleaTick.findAll(By(FleaTick.name, "ZoGuard Plus for Cats"))
      case "Adventure Plus for Dogs" =>
        FleaTick.findAll(By(FleaTick.name, "Adventure Plus for Dogs"))
      case "Adventure Plus for Cats" =>
        FleaTick.findAll(By(FleaTick.name, "Adventure Plus for Cats"))
      case "ShieldTec Plus for Dogs" =>
        FleaTick.findAll(By(FleaTick.name, "ShieldTec Plus for Dogs"))
    }).headOption

    val ratingRaw = Columns.cellValue(Columns.Rating, headerIndex, fieldList).openOr("")

    val rating = tryo(ratingRaw.toDouble).openOr(0d)

    val dateRaw    = Columns.cellValue(Columns.Date, headerIndex, fieldList).openOr("")
    val dateFormat = `Jan 01, 2021`._1
    val date       = dateFormat.parse(dateRaw).toZonedDateTime

    product.map { prod =>
      Review.create
        .title(title)
        .body(body)
        .rating(rating)
        .author(author)
        .date(date)
        .fleaTick(prod)
    }
  }

  def updateProductRatings: List[FleaTick] = {
    updateRating("ZoGuard Plus for Dogs")
    updateRating("ZoGuard Plus for Cats")
    updateRating("Adventure Plus for Dogs")
    updateRating("Adventure Plus for Cats")
    updateRating("ShieldTec Plus for Dogs")
  }

  def updateRating(productName: String): List[FleaTick] = {
    val fleaTicks = FleaTick.findAll(By(FleaTick.name, productName))
    val reviews   = fleaTicks.map(_.reviews.toList).flatten
    val ratings   = reviews.map(_.rating.get)
    val avgRating = ratings.sum / reviews.size

    fleaTicks.map { fleaTick => fleaTick.rating(avgRating).reviewCount(reviews.size).saveMe }
  }

  object Columns extends Enumeration {
    val Title: HeaderValue =
      HeaderValue(name = "Title", default = Failure("missing required Title"))
    val Body: HeaderValue = HeaderValue(name = "Body", default = Failure("missing required Email"))
    val Rating: HeaderValue =
      HeaderValue(name = "Rating", default = Failure("missing required Rating"))
    val Author: HeaderValue =
      HeaderValue(name = "Author", default = Failure("missing required Author"))
    val Date: HeaderValue = HeaderValue(name = "Date", default = Failure("missing required Date"))
    val Product: HeaderValue =
      HeaderValue(name = "Product", default = Failure("missing required Product"))

    def requiredColumnsCount: Int = {
      requiredColumns.size
    }

    def requiredColumns: Columns.ValueSet = {
      values.filter(_.required)
    }

    def missingRequiredHeaders(headerIndex: Map[Columns.Value, Int]): Columns.ValueSet = {
      requiredColumns.filter(headerIndex.get(_).isEmpty)
    }

    def cellBoolean(
        column: HeaderValue,
        headerIndex: Map[Value, Int],
        row: Array[String]
    ): Boolean = {
      cellValue(column, headerIndex, row).map(_.toLowerCase) match {
        case Full("1") | Full("y") | Full("yes") => true
        case _                                   => false
      }
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

    implicit def valueToHeaderValue(v: Value): HeaderValue = v.asInstanceOf[HeaderValue]
  }
}
