package com.mypetdefense.service

import com.mypetdefense.model._
import com.mypetdefense.snippet._
import net.liftweb.common.Box.box2Iterable
import net.liftweb.common._
import net.liftweb.http.S
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._

import java.text.SimpleDateFormat
import java.util.Date
import scala.util.matching.Regex

object ValidationService extends Loggable {
  val emailRegex: Regex = """^([^@]+)@([^@]+\.([^@].?)+)$""".r

  def checkEmpty(fieldValue: Option[String], fieldId: String): Box[ValidationError] = {
    checkEmpty(fieldValue getOrElse "", fieldId)
  }

  def checkEmpty(fieldValue: String, fieldId: String): Box[ValidationError] = {
    if (fieldValue.nonEmpty) {
      Empty
    } else {
      Full(ValidationError(fieldId, "Required."))
    }
  }

  def checkEmpty(fieldValue: Box[Any], fieldId: String): Box[ValidationError] = {
    if (fieldValue.nonEmpty) {
      Empty
    } else {
      Full(ValidationError(fieldId, "Required."))
    }
  }

  def checkDuplicateIpAddress(ipAddress: String, fieldId: String): Box[ErrorMessage] = {
    User.find(By(User.ipAddress, ipAddress)) match {
      case Full(_) => Full(ErrorMessage(fieldId, S ? "Coupon already applied for this address. Coupon has been removed."))
      case _       => Empty
    }
  }

  private def checkDuplicateEmail(email: String, errorId: String): Box[ValidationError] = {
    if (email.nonEmpty) {
      User.find(By(User.email, email)) match {
        case Full(_) => Full(ValidationError(errorId, S ? "Email already exists."))
        case _       => Empty
      }
    } else {
      Empty
    }
  }

  private def checkDuplicateEmailWithSubscription(
      email: String,
      errorId: String
  ): Box[ValidationError] = {
    if (email.nonEmpty) {
      User.find(By(User.email, email)).flatMap(_.subscription.obj) match {
        case Full(_) => Full(ValidationError(errorId, S ? "Email already exists."))
        case _       => Empty
      }
    } else {
      Empty
    }
  }

  private def checkDuplicateFacebookId(
      facebookId: String,
      errorId: String
  ): Box[ValidationError] = {
    if (facebookId.nonEmpty) {
      User.find(By(User.facebookId, facebookId)) match {
        case Full(_) => Full(ValidationError(errorId, S ? "Facebook account already exists."))
        case _       => Empty
      }
    } else {
      Empty
    }
  }

  private def checkDuplicateFacebookIdWithSubscription(
      facebookId: String,
      errorId: String
  ): Box[ValidationError] = {
    if (facebookId.nonEmpty) {
      User.find(By(User.facebookId, facebookId)).flatMap(_.subscription.obj) match {
        case Full(_) => Full(ValidationError(errorId, S ? "Facebook account already exists."))
        case _       => Empty
      }
    } else {
      Empty
    }
  }

  def checkDuplicateCoupon(coupon: String, errorId: String): Box[ValidationError] = {
    if (coupon.nonEmpty) {
      Coupon.find(By(Coupon.couponCode, coupon)) match {
        case Full(_) => Full(ValidationError(errorId, S ? "Code already exists."))
        case _       => Empty
      }
    } else {
      Empty
    }
  }

  def validEmailFormat(email: String, errorId: String): Box[ValidationError] = {
    val badEmail = Full(ValidationError(errorId, S ? "Not valid email address."))
    if (email.nonEmpty) {
      emailRegex.findFirstIn(email.trim) map { _ =>
        {
          //  check by trying to use it
          tryo {
            val address = new javax.mail.internet.InternetAddress(email)
            address.validate()
            Empty
          } openOr badEmail
        }
      } getOrElse {
        badEmail
      }
    } else {
      Full(ValidationError(errorId, S ? "Required."))
    }
  }

  def validNumber(number: String, errorId: String): Box[ValidationError] = {
    tryo(number.toDouble) match {
      case Full(_) => Empty
      case _       => Full(ValidationError(errorId, "Not a valid number."))
    }
  }

  def checkNumber(number: String, errorId: String): Box[ValidationError] = {
    checkEmpty(number, errorId) or
      validNumber(number, errorId)
  }

  def checkEmail(email: String, errorId: String, signup: Boolean = false): Box[ValidationError] = {
    {
      if (signup)
        checkDuplicateEmailWithSubscription(email, errorId)
      else
        checkDuplicateEmail(email, errorId)
    } or
      checkEmpty(email, errorId) or
      validEmailFormat(email, errorId)
  }

  def checkFacebookId(
      facebookId: String,
      errorId: String,
      signup: Boolean = false
  ): Box[ValidationError] = {
    {
      if (signup)
        checkDuplicateFacebookIdWithSubscription(facebookId, errorId)
      else
        checkDuplicateFacebookId(facebookId, errorId)
    } or
      checkEmpty(facebookId, errorId)
  }

  def checkCouponValue(value: String, errorId: String): Box[ValidationError] = {
    if (value.trim() == "0") {
      Full(ValidationError(errorId, "Cannot be 0."))
    } else {
      Empty
    }
  }

  def checkMonthPercentDollar(
      months: (String, String),
      percent: (String, String),
      dollar: (String, String)
  ): List[Box[ValidationError]] = {
    val hasMonths_?  = months._1.nonEmpty
    val hasPercent_? = percent._1.nonEmpty
    val hasDollar_?  = dollar._1.nonEmpty

    (hasMonths_?, hasPercent_?, hasDollar_?) match {
      case (false, false, false) =>
        List(
          Full(ValidationError(months._2, S ? "One of these is required.")),
          Full(ValidationError(percent._2, S ? "One of these is required.")),
          Full(ValidationError(dollar._2, S ? "One of these is required."))
        )

      case (_, true, true) =>
        List(
          Full(ValidationError(percent._2, S ? "One of these must be empty.")),
          Full(ValidationError(dollar._2, S ? "One of these must be empty."))
        )

      case (false, true, false) =>
        List(Empty)

      case (false, false, true) =>
        List(Empty)

      case (true, false, false) =>
        List(
          Full(ValidationError(percent._2, S ? "Need a percent or dollar with months.")),
          Full(ValidationError(dollar._2, S ? "Need a percent or dollar with months."))
        )

      case (true, false, true) =>
        List(Empty)

      case (true, true, false) =>
        List(Empty)
    }
  }

  def checkNonZero(
      quantity1: Int,
      quantity2: Int,
      field1: String,
      field2: String
  ): List[Box[ValidationError]] = {
    val number1 = tryo(quantity1.toInt).openOr(0)
    val number2 = tryo(quantity2.toInt).openOr(0)

    if (number1 <= 0 && number2 <= 0) {
      List(
        Full(ValidationError(field1, S ? "Need at least one item ordered.")),
        Full(ValidationError(field2, S ? "Need at least one item ordered."))
      )
    } else {
      List(Empty)
    }
  }

  def validDate(
      date: String,
      dateFormat: SimpleDateFormat,
      errorId: String
  ): Box[ValidationError] = {
    dateFormat.setLenient(false)

    if (date.isEmpty) {
      Empty
    } else {
      tryo(dateFormat.parse(date)) match {
        case Full(_) => Empty
        case _       => Full(ValidationError(errorId, "Not a valid date format."))
      }
    }
  }

  def checkBirthday(
      birthday: String,
      dateFormat: SimpleDateFormat,
      errorId: String
  ): Box[ValidationError] = {
    validDate(birthday, dateFormat, errorId)
  }

  def futureDate(
      date: String,
      dateFormat: SimpleDateFormat,
      now: Date,
      fieldId: String
  ): Box[ValidationError] = {
    Box.tryo(dateFormat.parse(date)).map(_.after(now)) match {
      case Full(true)  => Empty
      case Full(false) => Full(ValidationError(fieldId, "Not a future date."))
      case _           => Full(ValidationError(fieldId, "Not a valid date format."))
    }
  }

}

case class ValidationError(fieldSelector: String, error: String)
    extends MyPetDefenseEvent("form-validation-error")

case class ErrorMessage(fieldSelector: String, error: String)
  extends MyPetDefenseEvent("error-message")
