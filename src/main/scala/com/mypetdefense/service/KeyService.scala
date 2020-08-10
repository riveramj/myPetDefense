package com.mypetdefense.service

import scala.util.Random

import java.security.SecureRandom

import net.liftweb._
  import common._
  import mapper._

import net.liftweb.util.SecurityHelpers._

import org.joda.time.DateTime

import com.mypetdefense.model.User
import net.liftweb.util.StringHelpers

object KeyService extends Loggable {
  private val rng = new Random(new SecureRandom)

  def createAccessKey(): String = {
    StringHelpers.randomString(16)
  }

  def createProductSalesKey(): String = {
    StringHelpers.randomString(16)
  }
  
  def createResetKey(user: User): User = {
    val key = StringHelpers.randomString(16)
    val curTime = new DateTime()
    user.resetPasswordKey(key).saveMe
  }

  def verifyKey(userId: Long, key: String, keyType: String): Boolean = {
    User.find(By(User.userId, userId)) match {
      case Full(user) =>
        logger.debug(s"key is ${key}")
        
        keyType match {
          case "accessKey" =>
            user.accessKey.get match {
              case possibleKey if possibleKey == key => true
              case _ => false
            }

          case "productSalesKey" =>
            user.productSalesKey.get match {
              case possibleKey if possibleKey == key => true
              case _ => false
            }

          case "resetPasswordKey" =>
            user.resetPasswordKey.get match {
              case possibleKey if possibleKey == key => true
              case _ => false
            }

          case _ => false
        }

      case err =>
        logger.error("no user found")
        false
    }
  }

  def removeKey(user: User, keyType: String): Serializable = {
    keyType match {
      case "accessKey" =>
        user.accessKey("").saveMe
      
      case "productSalesKey" =>
        user.productSalesKey("").saveMe

      case "resetPasswordKey" =>
        user.resetPasswordKey("").saveMe

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

      case _ => Empty
    }
  }
}

