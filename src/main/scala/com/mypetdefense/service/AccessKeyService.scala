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

object AccessKeyService extends Loggable {
  private val rng = new Random(new SecureRandom)

  def createAccessKey() = {
    StringHelpers.randomString(16)
  }

  def verifyAccessKey(userId: Long, key: String): Boolean = {
    User.find(By(User.userId, userId)) match {
      case Full(user) =>
        logger.debug(s"key is ${key}")
        
        user.accessKey.get match {
          case possibleKey if possibleKey == key => true
          case _ => false
        }

      case err =>
        logger.error("no user found")
        false
    }
  }

  def removeAccessKey(key: String) {
    findUserByKey(key) match {
      case Full(user) =>
        user.accessKey("").saveMe
      case error => logger.error(s"could not remove key {${key}}. Error: ${error}")
    }
  }

  def findUserByKey(key: String): Box[User] = {
    User.find(By(User.accessKey, key)) match {
      case Full(user) => Full(user)
      case error =>
        logger.error(s"Error: ${error}. Key is s{key}")
        Empty
    }
  }
}

