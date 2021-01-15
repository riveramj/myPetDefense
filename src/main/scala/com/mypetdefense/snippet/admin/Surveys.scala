package com.mypetdefense.snippet
package admin

import java.time.ZonedDateTime

import com.mypetdefense.AppConstants.DefaultTimezone
import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.service._
import com.mypetdefense.util.DateFormatters._
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.mapper.By
import net.liftweb.util.ClearNodes
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

object Surveys extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Surveys") / "admin" / "surveys" >>
    mpdAdmin >>
    loggedIn
}

class Surveys extends Loggable {
  val dateFormat = `Jan 01, 2021`

  def addCoupon(parent: User, updatedCoupon: Box[Coupon]): Nothing = {
    parent.coupon(updatedCoupon).saveMe

    ParentService.updateCoupon(parent.stripeId.get, updatedCoupon.map(_.couponCode.get))

    S.redirectTo(Parents.menu.loc.calcDefaultHref)
  }

  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
      ".surveys [class+]" #> "current" &
        "tbody" #> SHtml.idMemoize { renderer =>
          val parents =
            User.findAll(By(User.userType, UserType.Parent), By(User.status, Status.Active))

          ".parent-survey" #> parents.sortWith(_.name < _.name).map { parent =>
            val possibleSurvey = parent.survey.obj

            def sendSurvey() = {
              val newSurvey = Survey.createNewSurvey(parent)
              parent.survey(newSurvey).saveMe

              EmailActor ! SendFeedbackEmail(parent)

              renderer.setHtml
            }

            ".name *" #> parent.name &
              ".email *" #> parent.email & {
              possibleSurvey.map { survey =>
                val surveyCouponApplied = survey.couponApplied.get

                def applyCoupon() = {
                  ParentService.updateCoupon(parent.stripeId.get, Full("feedbacksurvey"))
                  survey.couponApplied(ZonedDateTime.now(DefaultTimezone)).saveMe
                  renderer.setHtml
                }

                ".survey-sent *" #> survey.sentDate.get.format(dateFormat) &
                  ".ratings *" #> survey.ratingGiven.get &
                  ".testimonial *" #> survey.testimonialGiven.get & {
                  if (surveyCouponApplied == null) {
                    ".coupon .apply-coupon [onclick]" #> ajaxInvoke(applyCoupon _)
                  } else {
                    ".coupon *" #> surveyCouponApplied.format(dateFormat) &
                      ".actions .resend-survey" #> ClearNodes
                  }
                } &
                  ".actions .send-survey" #> ClearNodes
              }.openOr {
                ".survey-sent *" #> ClearNodes &
                  ".ratings .ratings-given" #> ClearNodes &
                  ".testimonial .testimonial-given" #> ClearNodes &
                  ".coupon .apply-coupon" #> ClearNodes &
                  ".actions .resend-survey" #> ClearNodes
              } &
                ".actions .send [onclick]" #> ajaxInvoke(sendSurvey _)
            }
          }
        }
  }
}
