package com.mypetdefense.snippet 
package admin

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearNodes
import net.liftweb.http._
  import js.JsCmds._
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.actor._

object Surveys extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Surveys") / "admin" / "surveys" >>
    adminUser >>
    loggedIn
}

class Surveys extends Loggable {
  val dateFormat = new SimpleDateFormat("MMM dd, YYYY")

  def addCoupon(parent: User, updatedCoupon: Box[Coupon]) = {
    parent.coupon(updatedCoupon).saveMe

    ParentService.updateCoupon(parent.stripeId.get, updatedCoupon.map(_.couponCode.get))

    S.redirectTo(Parents.menu.loc.calcDefaultHref)
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".surveys [class+]" #> "current" &
    "tbody" #> SHtml.idMemoize { renderer =>
      val parents = User.findAll(By(User.userType, UserType.Parent), By(User.status, Status.Active))

      ".parent-survey" #> parents.map { parent =>
        val possibleSurvey = parent.survey.obj

        def sendSurvey() = {
          val newSurvey = Survey.createNewSurvey(parent)
          parent.survey(newSurvey).saveMe

          EmailActor ! SendFeedbackEmail(parent)

          renderer.setHtml
        }

        ".name *" #> parent.name &
        ".email *" #> parent.email &
        {
          possibleSurvey.map { survey =>
            val completedSurvey = survey.ratingGiven.get || survey.testimonialGiven.get
            ".survey-sent *" #> survey.sentDate.toString &
            ".ratings *" #> survey.ratingGiven.get &
            ".testimonial *" #> survey.testimonialGiven.get &
            //".coupon *" #> surveycouponApplied.toString &
            ".actions .send-survey" #> ClearNodes &
            ".actions .resend-survey" #> ClearNodesIf(completedSurvey)
          }.openOr {
            ".survey-sent *" #> ClearNodes &
            ".ratings .ratings-given" #> ClearNodes &
            ".testimonial .testimonial-given" #> ClearNodes &
            ".coupon .apply-coupon" #> ClearNodes &
            ".actions .resend-survey" #> ClearNodes
          } &
          ".actions .send [onclick]" #> ajaxInvoke(sendSurvey)
        }
      }
    }
  }
}
