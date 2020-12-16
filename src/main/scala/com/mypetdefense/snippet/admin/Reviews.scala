package com.mypetdefense.snippet
package admin

import java.text.SimpleDateFormat

import com.mypetdefense.model._
import com.mypetdefense.util.RandomIdGenerator._
import com.mypetdefense.util._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

object Reviews extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Reviews") / "admin" / "reviews" >>
    mpdAdmin >>
    loggedIn
}

class Reviews extends Loggable {
  var newReviews: List[Review]                    = Nil
  var newReviewsRenderer: Box[IdMemoizeTransform] = Empty

  val dateFormat = new SimpleDateFormat("MMM dd, yyyy")

  def fileUpload: NodeSeq => NodeSeq = {
    var fileHolder: Box[FileParamHolder] = Empty

    def uploadFile(file: FileParamHolder): JsCmd = {
      logger.info(
        "Received: %s [size=%d, type=%s]" format (file.fileName, file.length, file.mimeType)
      )
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
      "#review-upload" #> SHtml.fileUpload { fph => fileHolder = Full(fph) } andThen
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
              ".product *" #> review.fleaTick.map(_.name.get)
          }
        }
  }

  def render: NodeSeq => NodeSeq = {
    val allReviews = Review.findAll()

    SHtml.makeFormsAjax andThen
      ".reviews [class+]" #> "current" &
        ".review" #> allReviews.map { review =>
          ".title *" #> review.title.get &
            ".body *" #> review.body.get &
            ".rating *" #> review.rating.get &
            ".author *" #> review.author.get &
            ".date *" #> dateFormat.format(review.date.get) &
            ".product *" #> review.fleaTick.map(_.name.get)
        }
  }
}
