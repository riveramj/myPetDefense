package com.mypetdefense.service

import com.mypetdefense.model.User
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.StringHelpers

import java.security.SecureRandom
import scala.util.Random

object KeyService extends Loggable {
  private val rng = new Random(new SecureRandom)

  def createAccessKey(): String = {
    StringHelpers.randomString(16)
  }

  def createProductSalesKey(): String = {
    StringHelpers.randomString(16)
  }

  def createResetKey(user: User): User = {
    val key     = StringHelpers.randomString(16)
    user.resetPasswordKey(key).saveMe
  }

  def generateNewPreBillingKey(user: User): User = {
    val key     = StringHelpers.randomString(16)
    user.preBillingKey(key).saveMe
  }

  def verifyKey(userId: Long, key: String, keyType: String): Boolean = {
    User.find(By(User.userId, userId)) match {
      case Full(user) =>
        logger.debug(s"key is ${key}")

        keyType match {
          case "accessKey" =>
            user.accessKey.get match {
              case possibleKey if possibleKey == key => true
              case _                                 => false
            }

          case "productSalesKey" =>
            user.productSalesKey.get match {
              case possibleKey if possibleKey == key => true
              case _                                 => false
            }

          case "resetPasswordKey" =>
            user.resetPasswordKey.get match {
              case possibleKey if possibleKey == key => true
              case _                                 => false
            }

          case "preBillingKey" =>
            user.preBillingKey.get match {
              case possibleKey if possibleKey == key => true
              case _   => false
            }

          case _ => false
        }

      case err =>
        logger.error("no user found")
        false
    }
  }

  def removeKey(user: User, keyType: String): Box[User] = {
    keyType match {
      case "accessKey" =>
        Full(user.accessKey("").saveMe)

      case "productSalesKey" =>
        Full(user.productSalesKey("").saveMe)

      case "resetPasswordKey" =>
        Full(user.resetPasswordKey("").saveMe)

      case "preBillingKey" =>
        Full(user.preBillingKey("").saveMe())

      case _ => Empty
    }
  }

  def findUserByKey(key: String, keyType: String): Box[User] = {
    keyType match {
      case "accessKey" =>
        User.find(By(User.accessKey, key))

      case "productSalesKey" =>
        User.find(By(User.productSalesKey, key))

      case "resetPasswordKey" =>
        User.find(By(User.resetPasswordKey, key))

      case "preBillingKey" =>
        User.find(By(User.preBillingKey, key))

      case _ => Empty
    }
  }
}
