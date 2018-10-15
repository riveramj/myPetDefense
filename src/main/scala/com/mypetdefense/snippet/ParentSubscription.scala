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

  val confirmCancelMenu = Menu.i("Confirm Cancel") / "confirm-cancel" >>
    loggedIn >>
    parent

  val confirmResumeMenu = Menu.param[String](
      "Confirm Resume",
      "Confirm Resume",
      Full(_),
      string => string
    ) / "confirm-resume" >>
    loggedIn >>
    parent

    val successfulResumeMenu = Menu.i("Subscription Resumed") / "subscription-resumed" >>
    loggedIn >>
    parent

  val confirmPauseMenu = Menu.param[String](
      "Confirm Pause",
      "Confirm Pause",
      Full(_),
      string => string
    ) / "confirm-pause" >>
    loggedIn >>
    parent

    val successfulPauseMenu = Menu.i("Subscription Paused") / "subscription-paused" >>
    loggedIn >>
    parent
  
    val cancelSurveySubscriptionMenu = Menu.i("Cancellation Survey") / "cancel-survey"

  val surveyCompleteSubscriptionMenu = Menu.i("Survey Complete") / "survey-complete"

  object currentUserSubscription extends SessionVar[Box[Subscription]](Empty)
}

class ParentSubscription extends Loggable {
  val dateFormat = new SimpleDateFormat("MM/dd/yyyy")

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

  def cancelUserAccount() = {
    val pets: List[Pet] = user.map(_.pets.toList).openOr(Nil)

    pets.map(ParentService.removePet(user, _))

    user.map(ParentService.removeParent(_))

    user.map { parent =>
      EmailActor ! ParentCancelledAccountEmail(parent)
    }

    SecurityContext.logCurrentUserOut

    S.redirectTo(ParentSubscription.cancelSurveySubscriptionMenu.loc.calcDefaultHref)
  }

  def render = {
    val userSubscription = SecurityContext.currentUser.flatMap(_.getSubscription).flatMap(_.refresh)

    SHtml.makeFormsAjax andThen
    ".subscription a [class+]" #> "current" &
    "#user-email *" #> email &
    "#email" #> text(email, userEmail => email = userEmail.trim) &
    "#old-password" #> SHtml.password(oldPassword, oldPass => oldPassword = oldPass.trim) &
    "#new-password" #> SHtml.password(newPassword, newPass => newPassword = newPass.trim) &
    ".update-email" #> SHtml.ajaxSubmit("Save Changes", updateEmail _) &
    ".update-password" #> SHtml.ajaxSubmit("Save Changes", updatePassword _) &
    ".status *" #> userSubscription.map(_.status.get.toString)
  }

  def manage = {
    val userSubscription = SecurityContext.currentUser.flatMap(_.getSubscription).flatMap(_.refresh)

    var cancelAccount = false
    var pauseAccount = false
    var resumeAccount = false

    val currentStatus = userSubscription.map(_.status.get)

    currentStatus match {
      case Full(Status.Paused) => pauseAccount = true
      case Full(Status.Cancelled) => cancelAccount = true
      case Full(_) => resumeAccount = true
      case _ =>
    }
    ParentSubscription.currentUserSubscription(userSubscription)

    val currentNextShipDate = userSubscription.map(_.nextShipDate.get)

    var nextShipDate = currentNextShipDate.map(dateFormat.format(_)).getOrElse("")

    def confirmAction() = {
      if (cancelAccount)
        S.redirectTo(ParentSubscription.confirmCancelMenu.loc.calcDefaultHref)
      else if (pauseAccount)
        S.redirectTo(ParentSubscription.confirmPauseMenu.toLoc.calcHref(nextShipDate))
      else
        S.redirectTo(ParentSubscription.confirmResumeMenu.toLoc.calcHref(nextShipDate))
    }

    def updateSubscriptionStatus(action: String)() = {
      action match {
        case "cancel" =>
          cancelAccount = true
          pauseAccount = false
          resumeAccount = false

        case "pause" =>
          cancelAccount = false
          pauseAccount = true
          resumeAccount = false
        
        case _ =>
          cancelAccount = false
          pauseAccount = false
          resumeAccount = true
      }
      
      Noop
    }

    SHtml.makeFormsAjax andThen
    "#user-email *" #> email &
    ".subscription a [class+]" #> "current" &
    "#resume-account" #> ClearNodesIf(currentStatus != Status.Paused) &
    "#resume-account [onclick]" #> SHtml.ajaxInvoke(() => updateSubscriptionStatus("resume")) &
    "#pause-account [class+]" #> { if (pauseAccount) "selected" else "" } &
    "#pause-account .pause [class+]" #> { if (pauseAccount) "selected" else "" } &
    "#pause-account .next-shipment" #> text(nextShipDate, possibleShipDate => nextShipDate = possibleShipDate.trim) &
    "#pause-account [onclick]" #> SHtml.ajaxInvoke(() => updateSubscriptionStatus("pause")) &
    "#cancel-account [onclick]" #> SHtml.ajaxInvoke(() => updateSubscriptionStatus("cancel")) &
    ".continue-account-changes" #> SHtml.ajaxSubmit("Continue", confirmAction _)
  }

  def resumeSubscription = {
    val nextShipDate = Date.from(LocalDate.now(ZoneId.of("America/New_York")).atStartOfDay(ZoneId.of("America/New_York")).plusDays(1).toInstant())

    def confirmResume() = {
      val subscription = ParentSubscription.currentUserSubscription.is.flatMap(_.refresh)

      val newShipDate = dateFormat.format(nextShipDate)
      val updatedShipDateSubscription = subscription.map(_.nextShipDate(nextShipDate).status(Status.Active).saveMe)
      ParentSubscription.currentUserSubscription(updatedShipDateSubscription)
     
      for {
        parent <- user
        subscription <- updatedShipDateSubscription
      } yield {
        EmailActor ! ParentResumeSubscriptionEmail(parent, subscription)
      }

      S.redirectTo(ParentSubscription.successfulResumeMenu.loc.calcDefaultHref)
    }

    SHtml.makeFormsAjax andThen
    "#user-email *" #> email &
    ".subscription a [class+]" #> "current" &
    ".next-shipment *" #> dateFormat.format(nextShipDate) &
    ".confirm-resume" #> SHtml.ajaxSubmit("Resume Subscription", confirmResume _)
  }

  def successfulResume = {
    val subscription = ParentSubscription.currentUserSubscription.is.flatMap(_.refresh)
    val currentNextShipDate = subscription.map(_.nextShipDate.get)

    var nextShipDate = currentNextShipDate.map(dateFormat.format(_)).getOrElse("")

    "#user-email *" #> email &
    ".subscription a [class+]" #> "current" &
    ".next-shipment *" #> nextShipDate &
    ".to-account-overview [href]" #> AccountOverview.menu.loc.calcDefaultHref
  }

  def pauseSubscription = {
    val nextShipDate = ParentSubscription.confirmPauseMenu.currentValue.openOr("")

    def confirmPause() = {
      val subscription = ParentSubscription.currentUserSubscription.is.flatMap(_.refresh)

      val newShipDate = dateFormat.parse(nextShipDate)
      val updatedShipDateSubscription = subscription.map(_.nextShipDate(newShipDate).status(Status.Paused).saveMe)
      ParentSubscription.currentUserSubscription(updatedShipDateSubscription)
     
      for {
        parent <- user
        subscription <- updatedShipDateSubscription
      } yield {
        EmailActor ! ParentPauseSubscriptionEmail(parent, subscription)
      }

      S.redirectTo(ParentSubscription.successfulPauseMenu.loc.calcDefaultHref)
    }

    SHtml.makeFormsAjax andThen
    "#user-email *" #> email &
    ".subscription a [class+]" #> "current" &
    ".next-shipment *" #> nextShipDate &
    ".confirm-pause" #> SHtml.ajaxSubmit("Pause Subscription", confirmPause _)
  }

  def successfulPause = {
    val subscription = ParentSubscription.currentUserSubscription.is.flatMap(_.refresh)
    val currentNextShipDate = subscription.map(_.nextShipDate.get)

    var nextShipDate = currentNextShipDate.map(dateFormat.format(_)).getOrElse("")

    "#user-email *" #> email &
    ".subscription a [class+]" #> "current" &
    ".next-shipment *" #> nextShipDate &
    ".to-account-overview [href]" #> AccountOverview.menu.loc.calcDefaultHref
  }

  def confirmCancelSubscription = {
    SHtml.makeFormsAjax andThen
    "#user-email *" #> email &
    ".subscription a [class+]" #> "current" &
    ".confirm-pause" #> SHtml.ajaxSubmit("Pause Subscription", cancelUserAccount _)
    
  }

  def survey = {
    val updatedSubscription = ParentSubscription.currentUserSubscription.is.flatMap(_.refresh)

    var selectedReason = ""
    var additionalComments = ""

    val cancelReasons = List(
      "I use a chewable pill.",
      "I don't use flea and tick at all.",
      "My vet said I don't need this.",
      "I like my current product more.",
      "Other."
    )

    def submitSurvey() = {
      updatedSubscription.map(_.cancellationReason(selectedReason).cancellationComment(additionalComments).saveMe)

      S.redirectTo(ParentSubscription.surveyCompleteSubscriptionMenu.loc.calcDefaultHref)
    }

    val cancelChoices = SHtml.radio(List("Pause", "Cancel"), Empty, selectedReason = _).toForm 

    SHtml.makeFormsAjax andThen
    ".sign-out" #> ClearNodes &
    ".survey-answer" #> cancelChoices.map { radio =>
      "input" #> radio
    } &
    ".comments" #> SHtml.textarea(additionalComments, additionalComments = _) &
    ".submit-survey" #> SHtml.ajaxSubmit("Submit Survey", submitSurvey _)
  }
}
