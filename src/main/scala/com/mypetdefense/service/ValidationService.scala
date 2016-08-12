package com.mypetdefense.service

import net.liftweb._ 
  import common._
  import http.S
  import util.Helpers._
  import mapper.By

import com.mypetdefense.model._

object ValidationService extends Loggable {
 val emailRegex = """^([^@]+)@([^@]+\.([^@].?)+)$""".r

  def checkEmpty(fieldValue: Option[String], fieldId: String): Box[ValidationError] = {
    checkEmpty(fieldValue getOrElse "", fieldId)
  }

  def checkEmpty(fieldValue: String, fieldId: String): Box[ValidationError] = {
    if (fieldValue.nonEmpty) {
      Empty
    } else {
      Full(ValidationError(fieldId, S ? "Field Required"))
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

  def checkEmail(email: String, errorId: String): Box[ValidationError] = {
    checkDuplicateEmail(email, errorId) or
    checkEmpty(email, errorId) or
    validEmailFormat(email, errorId)
  }
}

case class ValidationError(id: String, message: String)
