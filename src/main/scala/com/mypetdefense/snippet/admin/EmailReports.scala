package com.mypetdefense.snippet
package admin

import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers._

import scala.xml.{Elem, NodeSeq}

object EmailReports extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Email Reports") / "admin" / "email-reports" >>
    mpdAdmin >>
    loggedIn
}

class EmailReports extends Loggable {
  val emailReports: List[EmailReport]     = EmailReport.findAll()
  val emailReportRecords: List[EmailReportRecord] = EmailReportRecord.findAll()

  var email = ""
  var chosenReport: Box[EmailReport] = Empty

  def emailReportDropdown: Elem = {
    SHtml.selectObj(
      List((Empty, "")) ++ emailReports.map(report => (Full(report), report.name.get)),
      Full(chosenReport),
      (report: Box[EmailReport]) => chosenReport = report
    )
  }

  def createReportRecord: JsCmd = {
    val validateFields = List(
      checkEmail(email, "#assign-email")
    ).flatten

    if (validateFields.isEmpty) {
      val possibleUser = User.findByEmail(email)
      EmailReportRecord.createNewEmailReportRecord(
        chosenReport,
        possibleUser,
        email
      )
      S.redirectTo(EmailReports.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def deleteEmailReportRecord(emailReportRecord: EmailReportRecord)(): Alert = {
    if (emailReportRecord.delete_!)
      S.redirectTo(Coupons.menu.loc.calcDefaultHref)
    else
      Alert("An error has occurred. Please try again.")
  }

  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
    ".coupons [class+]" #> "current" &
    "#email" #> text(email, email = _) &
    "#report-descirption" #> chosenReport.map(_.description.get) &
    "#email-report-container #email-report-select" #> emailReportDropdown &
    "#create-item" #> SHtml.ajaxSubmit("Assign Report", () => createReportRecord) &
    ".report-record" #> emailReportRecords.map { reportRecord =>
      val report = reportRecord.emailReport.obj
      ".email *" #> reportRecord.email.get &
      ".user-name *" #> reportRecord.user.obj.map(_.name) &
      ".report-name *" #> report.map(_.name.get) &
      ".actions .delete [onclick]" #> Confirm(
        s"Delete ${reportRecord.email.get} for report ${report.map(_.name.get)}?",
        ajaxInvoke(deleteEmailReportRecord(reportRecord))
      )
    }
  }
}
