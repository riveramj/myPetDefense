package com.mypetdefense.util.csv

import java.text.SimpleDateFormat

import com.mypetdefense.model._
import net.liftweb.common._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._

object ReviewsUploadCSV extends GenericCSVParser[Review] {
  override def convertRowToModel(
      fieldList: Array[String],
      lineCount: Int,
      headerIndex: Map[Columns.Value, Int]
  ): Box[Review] = {
    import Columns._

    val title       = cellValue(Title, headerIndex, fieldList).openOr("")
    val body        = cellValue(Body, headerIndex, fieldList).openOr("")
    val author      = cellValue(Author, headerIndex, fieldList).openOr("")
    val productName = cellValue(Product, headerIndex, fieldList).openOr("")

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

    val ratingRaw = cellValue(Rating, headerIndex, fieldList).openOr("")

    val rating = tryo(ratingRaw.toDouble).openOr(0d)

    val dateRaw    = cellValue(Date, headerIndex, fieldList).openOr("")
    val dateFormat = new SimpleDateFormat("MMM dd, yyyy")
    val date       = dateFormat.parse(dateRaw)

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

  def updateProductRatings(): List[FleaTick] = {
    updateRating("ZoGuard Plus for Dogs")
    updateRating("ZoGuard Plus for Cats")
    updateRating("Adventure Plus for Dogs")
    updateRating("Adventure Plus for Cats")
    updateRating("ShieldTec Plus for Dogs")
  }

  def updateRating(productName: String): List[FleaTick] = {
    val fleaTicks = FleaTick.findAll(By(FleaTick.name, productName))
    val reviews   = fleaTicks.flatMap(_.reviews.toList)
    val ratings   = reviews.map(_.rating.get)
    val avgRating = ratings.sum / reviews.size

    fleaTicks.map { fleaTick => fleaTick.rating(avgRating).reviewCount(reviews.size).saveMe }
  }

  object Columns extends Columns {
    val Title: HeaderValue   = requiredValue("Title")
    val Body: HeaderValue    = requiredValue("Body")
    val Rating: HeaderValue  = requiredValue("Rating")
    val Author: HeaderValue  = requiredValue("Author")
    val Date: HeaderValue    = requiredValue("Date")
    val Product: HeaderValue = requiredValue("Product")
  }
}
