package com.mypetdefense.service

import net.liftweb._ 
  import common._
  import http.S
  import util.Helpers._
  import mapper.By

import com.mypetdefense.model._
import com.mypetdefense.snippet._

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
}

case class ValidationError(fieldSelector: String, error: String) extends MyPetDefenseEvent("form-validation-error")
