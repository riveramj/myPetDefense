package com.mypetdefense.model

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.mapper._

import java.util.Date

class MandrillTemplate extends LongKeyedMapper[MandrillTemplate] with IdPK with OneToMany[Long, MandrillTemplate] {
  def getSingleton: KeyedMetaMapper[Long, MandrillTemplate] = MandrillTemplate
  object mandrillTemplateId extends MappedLong(this) {
    override def dbIndexed_? = true
    override def defaultValue = generateLongId
  }
  object mandrillTemplateName extends MappedString(this, 20000)
  object templateName         extends MappedString(this, 20000) {
    override def dbIndexed_? = true
  }
  object emailType            extends MappedEnum(this, EmailType) {
    override def dbIndexed_? = true
  }
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object MandrillTemplate extends MandrillTemplate with LongKeyedMetaMapper[MandrillTemplate] {
  def createMandrillTemplate(
                              mandrillTemplateName: String,
                              templateName: String,
                              emailType: EmailType.Value
  ): MandrillTemplate = {
    MandrillTemplate.create
      .mandrillTemplateName(mandrillTemplateName)
      .templateName(templateName)
      .emailType(emailType)
      .saveMe
  }
}

object EmailType extends Enumeration {
  val Welcome, // 0
    NewAccountCreated, // 1
    PreBilling, // 2
    ShipmentMailed, // 3
    FailedPayment, // 4
    ForgotPassword, // 5
    PasswordReset, // 6
    CancelledAccount, // 7
    PausedAccount, // 8
    ResumedAccount, // 9
    ShipmentRefunded // 10
  = Value
}