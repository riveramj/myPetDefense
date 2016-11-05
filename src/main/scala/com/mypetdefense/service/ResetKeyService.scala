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

object ResetKeyService extends Loggable {
  private val rng = new Random(new SecureRandom)

  def createResetKey(user: User): User = {
    val key = StringHelpers.randomString(16)
    val curTime = new DateTime()
    user.resetPasswordKey(key).saveMe
  }

  def verifyResetKey(userId: Long, key: String): Boolean = {
    User.find(By(User.userId, userId)) match {
      case Full(user) =>
        logger.debug(s"key is ${key}")
        
        user.resetPasswordKey.get match {
          case possibleKey if possibleKey == key => true
          case _ => false
        }

      case err =>
        logger.error("no user found")
        false
    }
  }

  def removeResetKey(user: User) = {
    user.resetPasswordKey("").saveMe
  }

  def findUserByKey(key: String): Box[User] = {
    User.find(By(User.resetPasswordKey, key)) match {
      case Full(user) => Full(user)
      case error =>
        logger.error(s"Error: ${error}. Key is ${key}")
        Empty
    }
  }
}

