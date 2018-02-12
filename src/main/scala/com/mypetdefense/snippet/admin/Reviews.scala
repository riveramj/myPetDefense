package com.mypetdefense.snippet 
package admin

import net.liftweb._
  import sitemap.Menu
  import http.SHtml._
  import http._
  import js.JsCmds._

import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmd._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util._
import com.mypetdefense.service._
  import ValidationService._
import com.mypetdefense.actor._

import com.mypetdefense.util.RandomIdGenerator._

object Reviews extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Reviews") / "admin" / "reviews" >>
    adminUser >>
    loggedIn
}

class Reviews extends Loggable {
  var newReviews: List[Review] = Nil
  var newReviewsRenderer: Box[IdMemoizeTransform] = Empty
  
  val dateFormat = new SimpleDateFormat("MMM dd, yyyy")
  
  def fileUpload = {
    var fileHolder: Box[FileParamHolder] = Empty
    
    def uploadFile(file: FileParamHolder): JsCmd = {
      println("in upload")
      logger.info("Received: %s [size=%d, type=%s]" format(file.fileName, file.length, file.mimeType))
      val parsedFile = ReviewsUploadCSV.parse(file.file)
      newReviews = parsedFile.map(_.list).openOr(Nil)
      newReviewsRenderer.map(_.setHtml).openOr(Noop)
    }

    def createReviews() = {
      newReviews.map(_.reviewId(generateLongId).saveMe)
      ReviewsUploadCSV.updateProductRatings

      S.redirectTo(Reviews.menu.loc.calcDefaultHref)
    }

    SHtml.makeFormsAjax andThen
    "#review-upload" #> SHtml.fileUpload(fph => fileHolder = Full(fph)) andThen
    "#upload-reviews" #> SHtml.ajaxOnSubmit(() => {
      fileHolder.map(uploadFile) openOr {
        logger.error("Got unexpected Empty when handling partner file upload.")
        S.error("Missing file")
      }
    }) &
    "#create-reviews" #> SHtml.ajaxOnSubmit(createReviews _) &
    ".new-reviews" #> SHtml.idMemoize { renderer =>
      newReviewsRenderer = Full(renderer)

      ".new-review" #> newReviews.map { review =>
        ".title *" #> review.title.get &
        ".body *" #> review.body.get &
        ".rating *" #> review.rating.get &
        ".author *" #> review.author.get &
        ".date *" #> dateFormat.format(review.date.get) &
        ".product *" #> review.product.map(_.name.get)
      }
    }
  }

  def render = {
    val allReviews = Review.findAll()

    SHtml.makeFormsAjax andThen
    ".reviews [class+]" #> "current" &
    ".review" #> allReviews.map { review =>
      ".title *" #> review.title.get &
      ".body *" #> review.body.get &
      ".rating *" #> review.rating.get &
      ".author *" #> review.author.get &
      ".date *" #> dateFormat.format(review.date.get) &
      ".product *" #> review.product.map(_.name.get)
    }
  }
}
