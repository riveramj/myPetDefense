package com.mypetdefense.snippet

import com.mypetdefense.actor._
import com.mypetdefense.util.Paths
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

case class TestimonialSent() extends MyPetDefenseEvent("testimonial-sent")

class ContactUs extends Loggable {
  var name                         = ""
  var email                        = ""
  var testimonial                  = ""
  var comments                     = ""
  val sourcePage: String           = S.uri
  val isTestimonialPage: Boolean   = sourcePage == "/testimonial"
  var satisfactionRating: String   = ""
  var accuracyRating: String       = ""
  var convenientRating: String     = ""
  var recommendationRating: String = ""

  def sendMessage(): Nothing = {
    EmailActor ! TestimonialEmail(
      name,
      email,
      satisfactionRating,
      accuracyRating,
      convenientRating,
      recommendationRating,
      testimonial,
      comments
    )

    S.redirectTo(Paths.thanksPage.loc.calcDefaultHref)
  }

  def setRating(ratingName: String, ratingValue: String): Unit = {
    ratingName match {
      case "satisfaction" => satisfactionRating = ratingValue
      case "accuracy"     => accuracyRating = ratingValue
      case "convenient"   => convenientRating = ratingValue
      case _              => recommendationRating = ratingValue
    }
  }

  def render: NodeSeq => NodeSeq = {
    val productSatisfactionRatings = {
      ".product-satisfaction .strongly-disagree [onclick]" #> ajaxInvoke(() =>
        setRating("satisfaction", "Strongly Disagree")
      ) &
        ".product-satisfaction .disagree [onclick]" #> ajaxInvoke(() =>
          setRating("satisfaction", "Disagree")
        ) &
        ".product-satisfaction .neutral [onclick]" #> ajaxInvoke(() =>
          setRating("satisfaction", "Neutral")
        ) &
        ".product-satisfaction .agree [onclick]" #> ajaxInvoke(() =>
          setRating("satisfaction", "Agree")
        ) &
        ".product-satisfaction .strongly-agree [onclick]" #> ajaxInvoke(() =>
          setRating("satisfaction", "Strongly Agree")
        )
    }

    val accuracyRatings = {
      ".accuracy .strongly-disagree [onclick]" #> ajaxInvoke(() =>
        setRating("accuracy", "Strongly Disagree")
      ) &
        ".accuracy .disagree [onclick]" #> ajaxInvoke(() => setRating("accuracy", "Disagree")) &
        ".accuracy .neutral [onclick]" #> ajaxInvoke(() => setRating("accuracy", "Neutral")) &
        ".accuracy .agree [onclick]" #> ajaxInvoke(() => setRating("accuracy", "Agree")) &
        ".accuracy .strongly-agree [onclick]" #> ajaxInvoke(() =>
          setRating("accuracy", "Strongly Agree")
        )
    }

    val convenientRatings = {
      ".convenient .strongly-disagree [onclick]" #> ajaxInvoke(() =>
        setRating("convenient", "Strongly Disagree")
      ) &
        ".convenient .disagree [onclick]" #> ajaxInvoke(() => setRating("convenient", "Disagree")) &
        ".convenient .neutral [onclick]" #> ajaxInvoke(() => setRating("convenient", "Neutral")) &
        ".convenient .agree [onclick]" #> ajaxInvoke(() => setRating("convenient", "Agree")) &
        ".convenient .strongly-agree [onclick]" #> ajaxInvoke(() =>
          setRating("convenient", "Strongly Agree")
        )
    }

    val recommendationRating = {
      ".recommendation .strongly-disagree [onclick]" #> ajaxInvoke(() =>
        setRating("recommendation", "Strongly Disagree")
      ) &
        ".recommendation .disagree [onclick]" #> ajaxInvoke(() =>
          setRating("recommendation", "Disagree")
        ) &
        ".recommendation .neutral [onclick]" #> ajaxInvoke(() =>
          setRating("recommendation", "Neutral")
        ) &
        ".recommendation .agree [onclick]" #> ajaxInvoke(() => setRating("recommendation", "Agree")) &
        ".recommendation .strongly-agree [onclick]" #> ajaxInvoke(() =>
          setRating("recommendation", "Strongly Agree")
        )
    }

    SHtml.makeFormsAjax andThen
      ".name" #> text(name, name = _) &
        ".email" #> text(email, email = _) &
        ".testimonial" #> textarea(testimonial, testimonial = _) &
        ".comments" #> textarea(comments, comments = _) &
        productSatisfactionRatings &
        accuracyRatings &
        convenientRatings &
        recommendationRating &
        "#send-message" #> ajaxSubmit("Submit", sendMessage _)
  }
}
