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
import java.time.{Instant, OffsetDateTime}
import java.time.format.DateTimeFormatter

import scala.language.implicitConversions
import scala.language.postfixOps
import java.util.Date

import com.mypetdefense.util.AmazonOrderUploadCsv.Columns

case class AmazonOrders(list: List[AmazonOrder])

object AmazonOrderUploadCsv extends Loggable {
    def parse(source: Array[Byte]): Box[AmazonOrders] = parse(new String(source, StandardCharsets.UTF_8))

    object Columns extends Enumeration {
      val AmazonOrderId: HeaderValue = HeaderValue(name="amazon-order-id", default=Failure("missing required amazon-order-id"))
      val BuyerEmail: HeaderValue = HeaderValue(name="buyer-email", default=Failure("missing required buyer-email"))
      val BuyerPhone: HeaderValue = HeaderValue(name="buyer-phone-number", default=Failure("missing required buyer-phone-number"))
      val ProductName: HeaderValue = HeaderValue(name="product-name", default=Failure("missing required product-name"))
      val QuantityPurchased: HeaderValue = HeaderValue(name="quantity-shipped", default=Failure("missing required quantity-shipped"))
      val ProductPrice: HeaderValue = HeaderValue(name="item-price", default=Failure("missing required item-price"))
      val ProductDiscount: HeaderValue = HeaderValue(name="item-promotion-discount", default=Failure("missing required item-promotion-discount"))
      val Carrier: HeaderValue = HeaderValue(name="carrier", default=Failure("missing required carrier"))

      val Sku: HeaderValue = HeaderValue(name="sku", default=Failure("missing required sku"))
      val PurchaseDate: HeaderValue = HeaderValue(name="purchase-date", default=Failure("missing required purchase-date"))
      val RecipientName: HeaderValue = HeaderValue(name="recipient-name", default=Failure("missing required recipient-name"))
      val ShipAddress1: HeaderValue = HeaderValue(name="ship-address-1", default=Failure("missing required ship-address-1"))
      val ShipAddress2: HeaderValue = HeaderValue(name="ship-address-2", default=Failure("missing required ship-address-2"))
      val ShipAddress3: HeaderValue = HeaderValue(name="ship-address-3", default=Failure("missing required ship-address-3"))
      val ShipCity: HeaderValue = HeaderValue(name="ship-city", default=Failure("missing required ship-city"))
      val ShipState: HeaderValue = HeaderValue(name="ship-state", default=Failure("missing required ship-state"))
      val ShipZip: HeaderValue = HeaderValue(name="ship-postal-code", default=Failure("missing required ship-postal-code"))
    

    case class HeaderValue(
        name: String,
        matcher: Box[Regex] = Empty,
        parser: String=>Box[String] = (rawCell: String) => Full(rawCell.trim).filter(_.nonEmpty),
        default: Box[String] = Empty
      ) extends Val(name) {
      val nameMatcher: Regex = matcher openOr ("""(?i)^\s*"""+Pattern.quote(name)+"""\s*$""").r
      def matches(candidate: String): Boolean = {
        nameMatcher.unapplySeq(candidate).isDefined
      }
      def required: Boolean = {
        default match {
          case Failure(_, _, _) => true
          case _ => false
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

    def cellValue(column: HeaderValue, headerIndex: Map[Value, Int], row: Array[String]): Box[String] = {
      Box {
        for {
          index <- headerIndex get column
          cell <- row.lift(index)
          value <- column.parser(cell)
        } yield {
          value
        }
      } or column.default
    }

    def cellInt(column: HeaderValue, headerIndex: Map[Value, Int], row: Array[String]): Box[Int] = {
      cellValue(column, headerIndex, row).flatMap(i => tryo(i.toInt))
    }

    def cellDouble(column: HeaderValue, headerIndex: Map[Value, Int], row: Array[String]): Box[Double] = {
      cellValue(column, headerIndex, row).flatMap(i => tryo(i.toDouble))
    }

    def cellBoolean(column: HeaderValue, headerIndex: Map[Value, Int], row: Array[String]): Boolean = {
      cellValue(column, headerIndex, row).map(_.toLowerCase) match {
        case Full("1") | Full("y") | Full("yes") => true
        case _ => false
      }
    }

    implicit def valueToHeaderValue(v: Value): HeaderValue = v.asInstanceOf[HeaderValue]
  }

  def parse(source: String): Box[AmazonOrders] = {
    val amazonOrders: List[Box[AmazonOrder]] = {
      val emptyMsg = "File was empty"
      val lines = source split ("\r\n|[\r\n]")
      val filteredLines = lines map (_ trim) filterNot (_ isEmpty)
      // User uploaded Empty File
      if (filteredLines isEmpty) {
        Failure(emptyMsg) :: Nil
      } else {
        try {
          val reader = new CSVReader(new StringReader(source), CSVParser.DEFAULT_SEPARATOR, CSVParser.DEFAULT_QUOTE_CHARACTER, 0)
          val allRows = reader.readAll().asScala

          val headers =
            allRows.take(1)(0).zipWithIndex.flatMap {
              case (header, i) =>
                Columns.values.find(_.matches(header.trim)).map(_->i)
            }

          val lastRequiredColumn =
            headers.filter(_._1.required).last._2

          val headerIndex = headers toMap

          val missingRequiredColumns = Columns.missingRequiredHeaders(headerIndex)

          if (missingRequiredColumns.size > 0)
          {
            Failure(("Required columns missing: %s") format missingRequiredColumns.map(_.name).mkString(", ")) :: Nil
          } else {
            val rowsToRead = allRows.drop(1)
            if (rowsToRead.isEmpty)
            {
              Failure(emptyMsg) :: Nil // File with only header
            } else {
              (
                rowsToRead.zipWithIndex map {
                  case (fieldList, lineCount) =>
                    logger.debug("line:%s Contents: %s" format(lineCount + 1, Text(fieldList.mkString(", "))))
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

    val errors = amazonOrders filter (_.isEmpty) filter (_ != Empty)

    if (errors.isEmpty) {
      val successOrders = amazonOrders collect {
        case Full(order) => order
      }
      Full(AmazonOrders(successOrders.toList))
    } else {
      val errorMessages: List[String] = errors.map(_ match {
        case Failure(msg, _, _) => msg
        case _ => ""
      }).filter(_.nonEmpty)
      Empty ~> errorMessages
    }
  }

  private def failLine(msg: String, line: Int) = {
    Failure("%s [line %d]" format (msg, line))
  }

  def parseLine(fieldList: Array[String], lineCount: Int, headerIndex: Map[Columns.Value, Int], lastRequiredColumn: Int): Box[AmazonOrder] = {
    if (fieldList.isEmpty || fieldList.foldLeft("")(_ + _).trim.isEmpty) {
      Empty
    } else {
      fieldList.length match {
        case 0 => Empty
        case length if length <= lastRequiredColumn => failLine(S.?("Not enough fields"), lineCount)
        case _ => createAmazonOrder(fieldList, lineCount, headerIndex)
      }
    }
  }

  def createAmazonOrder(fieldList: Array[String], lineCount: Int, headerIndex: Map[Columns.Value, Int]): Box[AmazonOrder] = {
    val amazonOrderId = Columns.cellValue(Columns.AmazonOrderId, headerIndex, fieldList).openOr("")
    val buyerEmail = Columns.cellValue(Columns.BuyerEmail,headerIndex, fieldList).openOr("")
    val buyerPhone = Columns.cellValue(Columns.BuyerPhone, headerIndex, fieldList).openOr("")
    val productName = Columns.cellValue(Columns.ProductName, headerIndex, fieldList).openOr("")
    val quantityPurchased = Columns.cellInt(Columns.QuantityPurchased, headerIndex, fieldList).openOr(0)
    val productPrice = Columns.cellDouble(Columns.ProductPrice, headerIndex, fieldList).openOr(0D)
    val productDiscount = Columns.cellDouble(Columns.ProductDiscount, headerIndex, fieldList).openOr(0D)
    val carrier = Columns.cellValue(Columns.Carrier, headerIndex, fieldList).openOr("")

    val sku = Columns.cellValue(Columns.Sku, headerIndex, fieldList).openOr("")
    val rawPurchaseDate = Columns.cellValue(Columns.PurchaseDate, headerIndex, fieldList).openOr("")
    val name = Columns.cellValue(Columns.RecipientName, headerIndex, fieldList).openOr("")
    val address1 = Columns.cellValue(Columns.ShipAddress1, headerIndex, fieldList).openOr("")
    val address2 = Columns.cellValue(Columns.ShipAddress2, headerIndex, fieldList).openOr("")
    val address3 = Columns.cellValue(Columns.ShipAddress3, headerIndex, fieldList).openOr("")
    val city = Columns.cellValue(Columns.ShipCity, headerIndex, fieldList).openOr("")
    val state = Columns.cellValue(Columns.ShipState, headerIndex, fieldList).openOr("")
    val zip = Columns.cellValue(Columns.ShipZip, headerIndex, fieldList).openOr("")

    val timeFormatter = DateTimeFormatter.ISO_DATE_TIME
    val offsetDate = OffsetDateTime.parse(rawPurchaseDate, timeFormatter)
    val purchaseDate = Date.from(Instant.from(offsetDate))

    val animalType = {
      if (productName.toLowerCase().contains("dogs"))
        AnimalType.Dog
      else
        AnimalType.Cat
    }

    Full( AmazonOrder.create
      .amazonOrderId(amazonOrderId)
      .email(buyerEmail)
      .phone(buyerPhone)
      .quantityPurchased(quantityPurchased)
      .productPrice(productPrice)
      .productDiscount(productDiscount)
      .carrier(carrier)
      .productName(productName)
      .sku(sku)
      .name(name)
      .address1(address1)
      .address2(address2)
      .address3(address3)
      .city(city)
      .state(state)
      .zip(zip)
      .animalType(animalType)
      .purchaseDate(purchaseDate)
    )
  }
}

