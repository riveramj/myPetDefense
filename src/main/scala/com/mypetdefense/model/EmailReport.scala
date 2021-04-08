package com.mypetdefense.model

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common.Box
import net.liftweb.mapper.{MappedLongForeignKey, _}

import java.util.Date

class EmailReport extends LongKeyedMapper[EmailReport] with IdPK with OneToMany[Long, EmailReport] {
  def getSingleton: KeyedMetaMapper[Long, EmailReport] = EmailReport
  object emailReportId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object name           extends MappedString(this, 1000)
  object description    extends MappedString(this, 1000)
  object reportType     extends MappedEnum(this, ReportType )
  object emailRecords extends MappedOneToMany(EmailReportRecord, EmailReportRecord.emailReport)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object EmailReport extends EmailReport with LongKeyedMetaMapper[EmailReport] {
  def createNewEmailReport(
                       name: String,
                       description: String,
                       reportType: ReportType.Value
                     ): EmailReport = {
    EmailReport.create
      .emailReportId(generateLongId)
      .name(name)
      .description(description)
      .reportType(reportType)
      .saveMe
  }
}

class EmailReportRecord extends LongKeyedMapper[EmailReportRecord] with IdPK with OneToMany[Long, EmailReportRecord] {
  def getSingleton: KeyedMetaMapper[Long, EmailReportRecord] = EmailReportRecord
  object emailReportRecordId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object emailReport  extends MappedLongForeignKey(this, EmailReport)
  object user         extends MappedLongForeignKey(this, User)
  object email        extends MappedString(this, 500)
  object createdAt    extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object EmailReportRecord extends EmailReportRecord with LongKeyedMetaMapper[EmailReportRecord] {
  def createNewEmailReportRecord(
                            emailReport: Box[EmailReport],
                            user: Box[User],
                            email: String,
                          ): EmailReportRecord = {
    EmailReportRecord.create
      .emailReportRecordId(generateLongId)
      .emailReport(emailReport)
      .user(user)
      .email(email)
      .saveMe
  }
}

object ReportType extends Enumeration {
  val DailyTPPAgentSalesReportEmail, //0
  DailyInternalReportEmail, //1
  NewSaleEmail, //2
  UpgradeSubscriptionEmail, //3
  MonthlyTPPAgentSalesReportEmail = Value //4
}
