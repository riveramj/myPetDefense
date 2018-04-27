package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb._
  import http.SHtml._
  import util._
  import util.Helpers._
  import common._
  import util.ClearClearable
  import http._
  import mapper.{By, NullRef}
  import js._
  import JsCmds._

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.SecurityContext
import com.mypetdefense.util.Paths._
import com.mypetdefense.actor._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.util.SecurityContext._
import com.mypetdefense.service._
  import ValidationService._

object ParentSubscription extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Subscription") / "subscription" >>
    loggedIn >>
    parent

  val manageSubscriptionMenu = Menu.i("Manage Subscription") / "manage-subscription" >>
    loggedIn >>
    parent

  val cancelSurveySubscriptionMenu = Menu.i("Cancellation Survey") / "cancel-survey"

  val surveyCompleteSubscriptionMenu = Menu.i("Survey Complete") / "survey-complete"

  object currentUserSubscription extends SessionVar[Box[Subscription]](Empty)
}

class ParentSubscription extends Loggable {
  val user = currentUser
  val stripeCustomerId = user.map(_.stripeId.get).openOr("")

  var email = user.map(_.email.get).openOr("")
  var oldPassword = ""
  var newPassword = ""

  def updateEmail() = {
    val validateFields = List(
        checkEmail(email, "#email")
      ).flatten

    if(validateFields.isEmpty) {
      for {
        user <- user
        } {
          user
            .email(email)
            .saveMe
        }
        S.redirectTo(ParentSubscription.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def updatePassword() = {
    val validateFields = List(
        checkEmpty(oldPassword, "#old-password"),
        checkEmpty(newPassword, "#new-password")
      ).flatten

    if(validateFields.isEmpty) {
      val passwordUpdated = (for {
        user <- user
      } yield {
        if (User.isCorrectPassword_?(oldPassword, user)) {
          User.setUserPassword(user, newPassword)
          true
        } else {
          false
        }
      }).openOr(false)

      if (passwordUpdated)
        S.redirectTo(ParentSubscription.menu.loc.calcDefaultHref)
      else
        ValidationError("#old-password", "Incorrect old password")

    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def cancelAccount() = {
    val pets: List[Pet] = user.map(_.pets.toList).openOr(Nil)

    pets.map(ParentService.removePet(user, _))

    user.map(ParentService.removeParent(_))

    if (Props.mode != Props.RunModes.Pilot) {
      user.map { parent =>
        EmailActor ! AccountCancelledEmail(parent) 
      }
    }

    SecurityContext.logCurrentUserOut

    S.redirectTo(ParentSubscription.cancelSurveySubscriptionMenu.loc.calcDefaultHref)
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".subscription a [class+]" #> "current" &
    "#user-email *" #> email &
    "#email" #> text(email, userEmail => email = userEmail.trim) &
    "#old-password" #> SHtml.password(oldPassword, oldPass => oldPassword = oldPass.trim) &
    "#new-password" #> SHtml.password(newPassword, newPass => newPassword = newPass.trim) &
    ".update-email" #> SHtml.ajaxSubmit("Save Changes", updateEmail _) &
    ".update-password" #> SHtml.ajaxSubmit("Save Changes", updatePassword _) &
    ".status *" #> user.map(_.status.get.toString)
  }

  def manage = {
    val userSubscription = SecurityContext.currentUser.flatMap(_.subscription.headOption)

    ParentSubscription.currentUserSubscription(userSubscription)

    SHtml.makeFormsAjax andThen
    "#user-email *" #> email &
    ".subscription a [class+]" #> "current" &
    ".cancel" #> SHtml.ajaxSubmit("Finish Cancellation", cancelAccount _)
  }

  def survey = {
    val updatedSubscription = ParentSubscription.currentUserSubscription.is.flatMap(_.refresh)

    var selectedReason = ""
    var additionalComments = ""

    val cancelReasons = List(
      "I use an oral brand.",
      "I don't use flea and tick at all.",
      "My vet said I don't need this.",
      "I like my current product more."
    )

    def submitSurvey() = {
      updatedSubscription.map(_.cancellationReason(selectedReason).cancellationComment(additionalComments).saveMe)

      S.redirectTo(ParentSubscription.surveyCompleteSubscriptionMenu.loc.calcDefaultHref)
    }

    val cancelChoices = SHtml.radio(cancelReasons, Empty, selectedReason = _).toForm 

    SHtml.makeFormsAjax andThen
    ".sign-out" #> ClearNodes &
    ".survey-answer" #> cancelChoices.map { radio =>
      "input" #> radio
    } &
    ".comments" #> SHtml.textarea(additionalComments, additionalComments = _) &
    ".submit-survey" #> SHtml.ajaxSubmit("Submit Survey", submitSurvey _)
  }
}

