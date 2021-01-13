package com.mypetdefense.snippet.customer

import java.text.SimpleDateFormat
import java.time.LocalDate
import java.util.Date

import com.mypetdefense.AppConstants.DefaultTimezone
import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import com.mypetdefense.util.DateHelper._
import com.mypetdefense.util.SecurityContext._
import com.mypetdefense.util.{ClearNodesIf, SecurityContext}
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._
import net.liftweb.util.Helpers._
import net.liftweb.util._

import scala.xml.NodeSeq

object ParentSubscription extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Subscription") / "subscription" >>
    loggedIn >>
    parent

  val manageSubscriptionMenu: Menu.Menuable =
    Menu.i("Manage Subscription") / "manage-subscription" >>
      loggedIn >>
      parent

  val confirmCancelMenu: Menu.Menuable = Menu.i("Confirm Cancel") / "confirm-cancel" >>
    loggedIn >>
    parent

  val confirmResumeMenu: Menu.ParamMenuable[String] = Menu.param[String](
    "Confirm Resume",
    "Confirm Resume",
    Full(_),
    string => string
  ) / "confirm-resume" >>
    loggedIn >>
    parent

  val successfulResumeMenu: Menu.Menuable =
    Menu.i("Subscription Resumed") / "subscription-resumed" >>
      loggedIn >>
      parent

  val confirmPauseMenu: Menu.ParamMenuable[String] = Menu.param[String](
    "Confirm Pause",
    "Confirm Pause",
    Full(_),
    string => string
  ) / "confirm-pause" >>
    loggedIn >>
    parent

  val successfulPauseMenu: Menu.Menuable = Menu.i("Subscription Paused") / "subscription-paused" >>
    loggedIn >>
    parent

  val cancelSurveySubscriptionMenu: Menu.Menuable with Menu.WithSlash =
    Menu.i("Cancellation Survey") / "cancel-survey"

  val surveyCompleteSubscriptionMenu: Menu.Menuable with Menu.WithSlash =
    Menu.i("Survey Complete") / "survey-complete"

  object currentUserSubscription extends SessionVar[Box[Subscription]](Empty)
}

class ParentSubscription extends Loggable {
  val dateFormat = new SimpleDateFormat("MM/dd/yyyy")

  val user: Box[User]          = currentUser
  val stripeCustomerId: String = user.map(_.stripeId.get).openOr("")

  var email: String = user.map(_.email.get).openOr("")
  var oldPassword   = ""
  var newPassword   = ""

  def updateEmail(): JsCmd = {
    val validateFields = List(
      checkEmail(email, "#email")
    ).flatten

    if (validateFields.isEmpty) {
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

  def updatePassword(): JsCmd = {
    val validateFields = List(
      checkEmpty(oldPassword, "#old-password"),
      checkEmpty(newPassword, "#new-password")
    ).flatten

    if (validateFields.isEmpty) {
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

  def cancelUserAccount(): Nothing = {
    val pets: List[Pet] = user.map(_.pets.toList).openOr(Nil)

    pets.map(ParentService.removePet(user, _))

    user.map(ParentService.removeParent(_))

    user.map { parent => EmailActor ! ParentCancelledAccountEmail(parent) }

    SecurityContext.logCurrentUserOut()

    S.redirectTo(ParentSubscription.cancelSurveySubscriptionMenu.loc.calcDefaultHref)
  }

  def render: NodeSeq => NodeSeq = {
    val userSubscription =
      SecurityContext.currentUser.flatMap(_.subscription.obj).map(_.reload)

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

  def manage: NodeSeq => NodeSeq = {
    val userSubscription =
      SecurityContext.currentUser.flatMap(_.subscription.obj).map(_.reload)

    var cancelAccount = false
    var pauseAccount  = false
    var resumeAccount = false

    val currentStatus = userSubscription.map(_.status.get)

    currentStatus match {
      case Full(Status.Paused)    => pauseAccount = true
      case Full(Status.Cancelled) => cancelAccount = true
      case Full(_)                => resumeAccount = true
      case _                      =>
    }
    ParentSubscription.currentUserSubscription(userSubscription)

    val currentNextShipDate = userSubscription.map(_.nextShipDate.get)

    var nextShipDate = currentNextShipDate.map(dateFormat.format).getOrElse("")

    def validateFields: List[ValidationError] = {
      val checkPause =
        if (!pauseAccount) Empty
        else
          futureDate(
            nextShipDate,
            dateFormat,
            nowAtStartOfDay.toDate,
            "#pause-account .next-shipment"
          )

      checkPause.toList
    }

    def confirmAction(): JsCmd = {
      val validationErrors = validateFields

      if (validationErrors.nonEmpty)
        validationErrors.foldLeft(Noop)(_ & _)
      else {
        if (cancelAccount)
          S.redirectTo(ParentSubscription.confirmCancelMenu.loc.calcDefaultHref)
        else if (pauseAccount)
          S.redirectTo(ParentSubscription.confirmPauseMenu.toLoc.calcHref(nextShipDate))
        else
          S.redirectTo(ParentSubscription.confirmResumeMenu.toLoc.calcHref(nextShipDate))
      }
    }

    def updateSubscriptionStatus(action: String) = {
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
        "#resume-account" #> ClearNodesIf(!(currentStatus contains Status.Paused)) &
        "#resume-account [onclick]" #> SHtml.ajaxInvoke(() => updateSubscriptionStatus("resume")) &
        "#pause-account [class+]" #> { if (pauseAccount) "selected" else "" } &
        "#pause-account .pause [class+]" #> { if (pauseAccount) "selected" else "" } &
        "#pause-account .next-shipment" #> text(
          nextShipDate,
          possibleShipDate => nextShipDate = possibleShipDate.trim
        ) &
        "#pause-account [onclick]" #> SHtml.ajaxInvoke(() => updateSubscriptionStatus("pause")) &
        "#cancel-account [onclick]" #> SHtml.ajaxInvoke(() => updateSubscriptionStatus("cancel")) &
        ".continue-account-changes" #> SHtml.ajaxSubmit("Continue", confirmAction _)
  }

  def resumeSubscription: NodeSeq => NodeSeq = {
    val nextShipDate = Date.from(
      LocalDate
        .now(DefaultTimezone)
        .atStartOfDay(DefaultTimezone)
        .plusDays(1)
        .toInstant
    )

    def confirmResume() = {
      val subscription = ParentSubscription.currentUserSubscription.is.map(_.reload)

      val updatedShipDateSubscription =
        subscription.map(_.nextShipDate(nextShipDate.toZonedDateTime).status(Status.Active).saveMe)
      ParentSubscription.currentUserSubscription(updatedShipDateSubscription)

      for {
        parent       <- user
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

  def successfulResume: CssBindFunc = {
    val subscription        = ParentSubscription.currentUserSubscription.is.map(_.reload)
    val currentNextShipDate = subscription.map(_.nextShipDate.get)

    val nextShipDate = currentNextShipDate.map(dateFormat.format).getOrElse("")

    "#user-email *" #> email &
      ".subscription a [class+]" #> "current" &
      ".next-shipment *" #> nextShipDate &
      ".to-account-overview [href]" #> AccountOverview.menu.loc.calcDefaultHref
  }

  def pauseSubscription: NodeSeq => NodeSeq = {
    val nextShipDate = ParentSubscription.confirmPauseMenu.currentValue.openOr("")

    def confirmPause(): Nothing = {
      val subscription = ParentSubscription.currentUserSubscription.is.map(_.reload)

      val newShipDate = dateFormat.parse(nextShipDate)
      val updatedShipDateSubscription = subscription.flatMap { sub =>
        val updatedSubscriptionWithStripe =
          ParentService.updateNextShipBillDate(sub, newShipDate)

        updatedSubscriptionWithStripe.map(_.status(Status.Paused).saveMe)
      }

      ParentSubscription.currentUserSubscription(updatedShipDateSubscription)

      for {
        parent       <- user
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

  def successfulPause: CssBindFunc = {
    val subscription        = ParentSubscription.currentUserSubscription.is.map(_.reload)
    val currentNextShipDate = subscription.map(_.nextShipDate.get)

    val nextShipDate = currentNextShipDate.map(dateFormat.format).getOrElse("")

    "#user-email *" #> email &
      ".subscription a [class+]" #> "current" &
      ".next-shipment *" #> nextShipDate &
      ".to-account-overview [href]" #> AccountOverview.menu.loc.calcDefaultHref
  }

  def confirmCancelSubscription: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
      "#user-email *" #> email &
        ".subscription a [class+]" #> "current" &
        ".confirm-cancel" #> SHtml.ajaxSubmit("Cancel Subscription", cancelUserAccount _)

  }

  def survey: NodeSeq => NodeSeq = {
    val updatedSubscription = ParentSubscription.currentUserSubscription.is.map(_.reload)

    var selectedReason     = ""
    var additionalComments = ""

    val cancelReasons = List(
      "I use a chewable pill.",
      "I don't use flea and tick at all.",
      "My vet said I don't need this.",
      "I like my current product more.",
      "Other."
    )

    def submitSurvey() = {
      updatedSubscription.map(
        _.cancellationReason(selectedReason).cancellationComment(additionalComments).saveMe
      )

      S.redirectTo(ParentSubscription.surveyCompleteSubscriptionMenu.loc.calcDefaultHref)
    }

    val cancelChoices = SHtml.radio(cancelReasons, Empty, selectedReason = _).toForm

    SHtml.makeFormsAjax andThen
      ".sign-out" #> ClearNodes &
        ".survey-answer" #> cancelChoices.map { radio => "input" #> radio } &
        ".comments" #> SHtml.textarea(additionalComments, additionalComments = _) &
        ".submit-survey" #> SHtml.ajaxSubmit("Submit Survey", submitSurvey _)
  }
}
