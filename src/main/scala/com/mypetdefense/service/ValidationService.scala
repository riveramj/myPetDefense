package com.mypetdefense.service

import net.liftweb._ 
  import common._
  import http.S
  import util.Helpers._
  import mapper.By

import com.mypetdefense.model._
import com.mypetdefense.snippet._

import java.util.Date
import java.text.SimpleDateFormat

object ValidationService extends Loggable {
 val emailRegex = """^([^@]+)@([^@]+\.([^@].?)+)$""".r

  def checkEmpty(fieldValue: Option[String], fieldId: String): Box[ValidationError] = {
    checkEmpty(fieldValue getOrElse "", fieldId)
  }

  def checkEmpty(fieldValue: String, fieldId: String): Box[ValidationError] = {
    if (fieldValue.nonEmpty) {
      Empty
    } else {
      Full(ValidationError(fieldId, "Required"))
    }
  }

  def checkDuplicateEmail(email: String, errorId: String): Box[ValidationError] = {
    if (email.nonEmpty) {
      User.find(By(User.email, email)) match {
        case Full(user)  => Full(ValidationError(errorId, S ? "Email already exists"))
        case _ => Empty
      }
    } else {
      Empty
    }
  }

  def checkDuplicateCoupon(coupon: String, errorId: String): Box[ValidationError] = {
    if (coupon.nonEmpty) {
      Coupon.find(By(Coupon.couponCode, coupon)) match {
        case Full(coupon)  => Full(ValidationError(errorId, S ? "Code already exists"))
        case _ => Empty
      }
    } else {
      Empty
    }
  }

  def validEmailFormat(email: String, errorId: String): Box[ValidationError] = {
    val badEmail = Full(ValidationError(errorId, S ? "Not valid email address"))
    if (email.nonEmpty) {
      emailRegex.findFirstIn(email.trim) map {
        _ => {
          //  check by trying to use it
          tryo {
            val address = new javax.mail.internet.InternetAddress(email)
            address.validate
            Empty
          } openOr badEmail
        }
      } getOrElse {
        badEmail
      }
    } else {
      Full(ValidationError(errorId, S ? "Field Required"))
    }
  }

  def validNumber(number: String, errorId: String): Box[ValidationError] = {
    tryo(number.toInt) match {
      case Full(realInt) => Empty
      case _ => Full(ValidationError(errorId, "Not a number"))
    }
  }

  def checkNumber(number: String, errorId: String): Box[ValidationError] = {
    checkEmpty(number, errorId) or
    validNumber(number, errorId)
  }

  def checkEmail(email: String, errorId: String): Box[ValidationError] = {
    checkDuplicateEmail(email, errorId) or
    checkEmpty(email, errorId) or
    validEmailFormat(email, errorId)
  }

  def checkMonthAndPercent(months: (String, String), percent: (String, String)) = {
    val hasMonths_? = months._1.nonEmpty
    val hasPercent_? = percent._1.nonEmpty

    (hasMonths_?, hasPercent_?) match {
      case (false, false) =>
        List(
          Full(ValidationError(months._2, S ? "One of these is required")),
          Full(ValidationError(percent._2, S ? "One of these is required"))
        )

      case (false, true) =>
        List(Empty)

      case (true, false) =>
        List(Full(ValidationError(percent._2, S ? "Need a percent with months")))

      case (true, true) =>
        List(Empty)
    }
  }
  
  def checkBirthdayFormat(birthday: String, dateFormat: SimpleDateFormat, errorId: String) = {
    dateFormat.setLenient(false)

    tryo(dateFormat.parse(birthday)) match {
      case Full(_) => Empty
      case _ => Full(ValidationError(errorId, "Not a validate date format."))
    }
  }

  def checkBirthday(birthday: String, dateFormat: SimpleDateFormat, errorId: String): Box[ValidationError] = {
    checkEmpty(birthday, errorId) or
    checkBirthdayFormat(birthday, dateFormat, errorId)
  }
}

case class ValidationError(fieldSelector: String, error: String) extends MyPetDefenseEvent("form-validation-error")
